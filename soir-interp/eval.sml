(* eval.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Eval : sig

    val apply : DynamicEnv.t * SOIR.func * Value.t list -> Value.t

  end = struct

    structure IR = SOIR
    structure V = Value
    structure DE = DynamicEnv

  (* exception used to return from a function *)
    exception Return of Value.t

  (* exception used to exit from a loop *)
    exception Exit

  (* exception used to signal a runtime error *)
    exception RuntimeError = DE.RuntimeError

  (* the first two slots of the metadata are the object size and super-class.  These
   * are not included in the metadata item array, so we need to adjust offsets.
   *)
    val mdOffset = 2

    fun handleExn (env, RuntimeError msg) = Runtime.failure msg
      | handleExn (env, ex) = Runtime.failure(concat["uncaught exception ", exnName ex])

    fun expected (env, ty, v) =
          DE.runtimeError(env, concat["expected ", ty, ", but got " ^ V.toString v])

  (* version of List.nth that raises RuntimeError if out of bounds *)
    fun nth (env, xs, i, msg) =
          (List.nth (xs, i) handle _ => DE.runtimeError(env, msg))

    fun apply (env, f, args) = (case DE.lookupFunc(env, f)
           of DE.UserFn(IR.FuncDef{params, body, ...}) => (
                DE.traceCall (env, f, args);
                (evalBlk (DE.bindParams (env, params, args), body);
                  raise RuntimeError "missing return in function"
(* FIXME: check void return *)
                ) handle Return v => (DE.traceReturn env; v))
            | DE.PrimFn f => f args
          (* end case *))
            handle ex => handleExn (env, ex)

    and evalBlk (env, IR.Block{stms, ...}) = let
          fun evalStms (env, []) = ()
            | evalStms (env, stm::stms) = let
                fun continue env = evalStms (env, stms)
                in
                  DE.traceStm (env, stm);
                  case stm
                   of IR.LabelStm _ => raise Fail "unexpected LabelStm"
                    | IR.LocalVarStm(x, NONE) => continue (DE.bindVar(env, x, V.UNDEF))
                    | IR.LocalVarStm(x, SOME e) => (case evalExp(env, e)
                         of V.UNDEF => expected (env, "value", V.UNDEF)
                          | v => continue (DE.bindVar(env, x, v))
                        (* end case *))
                    | IR.LocalAssignStm(x, e) => (case evalExp(env, e)
                         of V.UNDEF => expected (env, "value", V.UNDEF)
                          | v => (DE.lookupVar(env, x) := v; continue env)
                        (* end case *))
                    | IR.AssignStm(obj, x as IR.MVAR{name, offset, ...}, e) => (
                        case evalValue(env, obj)
                         of V.OBJ data => (case evalExp(env, e)
                               of V.UNDEF => expected (env, "value", V.UNDEF)
                                | v => if (offset = 0)
                                      then DE.runtimeError(env, "assignment to \"_md\" of object")
                                    else if ((1 <= offset) andalso (offset < Array.length data))
                                      then (Array.update(data, offset, v); continue env)
                                      else DE.runtimeError(env, concat[
                                          "offset for ", name, " is out of bounds"
                                        ])
                              (* end case *))
                          | v => expected (env, "object", v)
                        (* end case *))
                    | IR.NewStm(x, md as IR.CLASS{vars, ...}) => let
                        val obj = Array.array(List.length vars + 1, V.UNDEF)
                        in
                          Array.update(obj, 0, V.META md);
                          continue (DE.bindVar(env, x, V.OBJ obj))
                        end
                    | IR.TupleStm(x, args) =>
                        continue (DE.bindVar(env, x, V.TUPLE(evalValues(env, args))))
                    | IR.ApplyStm(x, f, args) => (case evalValue(env, f)
                         of V.FUNC f => (case apply(env, f, evalValues(env, args))
                               of V.VOID => expected (env, "value", V.VOID)
                                | V.UNDEF => expected (env, "value", V.UNDEF)
                                | v => continue (DE.bindVar(env, x, v))
                              (* end case *))
                          | v => expected (env, "function", v)
                        (* end case *))
                    | IR.CallStm(f, args) => (case evalValue(env, f)
                         of V.FUNC f => (case apply(env, f, evalValues(env, args))
                               of V.VOID => continue env
                                | v => expected (env, "void", v)
                              (* end case *))
                          | v => expected (env, "function", v)
                        (* end case *))
                    | IR.IfStm(v, blk, optBlk) => (case (evalValue(env, v), optBlk)
                         of (V.BOOL true, _) => (evalBlk(env, blk); continue env)
                          | (V.BOOL false, SOME blk) => (evalBlk(env, blk); continue env)
                          | (V.BOOL false, NONE) => continue env
                          | (v, _) => expected (env, "boolean", v)
                        (* end case *))
                    | IR.LoopStm blk => let
                        fun lp () = (evalBlk (env, blk); DE.traceStm (env, stm); lp())
                        in
                          lp () handle Exit => continue env
                        end
                    | IR.ExitIfStm v => (case evalValue(env, v)
                         of V.BOOL true => raise Exit
                          | V.BOOL false => continue env
                          | v => expected (env, "boolean", v)
                        (* end case *))
                    | IR.ReturnStm NONE => raise Return V.VOID
                    | IR.ReturnStm(SOME v) => raise Return(evalValue(env, v))
                  (* end case *)
                end
          in
            evalStms (env, stms)
          end

  (* evaluate an IR expression *)
    and evalExp (env, e) = (case e
           of IR.ValExp v => evalValue (env, v)
            | IR.MemberVarExp(obj, x as IR.MVAR{name, offset, ...}) => (
                case evalValue(env, obj)
                 of V.OBJ data => if ((0 <= offset) andalso (offset < Array.length data))
                      then Array.sub(data, offset)
                      else DE.runtimeError(env, concat["offset for ", name, " is out of bounds"])
                  | v => expected (env, "object", v)
                (* end case *))
            | IR.MetaSel(v, (name, ix)) => (case evalValue(env, v)
                 of V.META(IR.CLASS{metadata, ...}) => (
                      case nth(env, metadata, ix-mdOffset, "metadata selection is out of bounds")
                       of IR.FuncItem f => V.FUNC f
                        | IR.IndexItem iTbl => V.INDEX iTbl
                      (* end case *))
                  | V.INDEX(IR.INDEX{items, ...}) => let
                      val (_, i) = nth(env, !items, ix, "index selection is out of bounds")
                      in
                        V.INT(IntInf.fromInt i)
                      end
                  | v => expected (env, "metadata", v)
                (* end case *))
            | IR.IndexSel(v, ix) => (case evalValue(env, v)
                 of V.META(IR.CLASS{metadata, ...}) => (case evalValue(env, ix)
                       of V.INT ix => (
                            case nth(env, metadata, Int.fromLarge(ix)-mdOffset,
                                "metadata selection is out of bounds")
                             of IR.FuncItem f => V.FUNC f
                              | IR.IndexItem idx => V.INDEX idx
                            (* end case *))
                        | v => expected (env, "integer", v)
                      (* end case *))
                  | v => expected (env, "metadata", v)
                (* end case *))
               (* `v[v]` -- index metadata *)
            | IR.PrimExp(rator, args) => evalPrim (rator, evalValues(env, args))
            | IR.TupleSel(i, v) => (case evalValue(env, v)
                 of V.TUPLE vs => nth(env, vs, i-1, "tuple selection is out of bounds")
                  | v => expected (env, "tuple", v)
                (* end case *))
          (* end case *))

    and evalPrim (rator, args) = let
          fun mkInt n = if (n < ~4611686018427387904) orelse (4611686018427387903 < n)
                then raise RuntimeError "integer result out of range"
                else V.INT n
          in
            case (rator, args)
             of (PrimOp.isNil, [V.NIL]) => V.BOOL true
              | (PrimOp.isNil, [_]) => V.BOOL false
              | (PrimOp.isNotNil, [V.NIL]) => V.BOOL false
              | (PrimOp.isNotNil, [_]) => V.BOOL true
              | (PrimOp.BoolNot, [V.BOOL b]) => V.BOOL(not b)
              | (PrimOp.BoolEqu, [V.BOOL b1, V.BOOL b2]) => V.BOOL(b1 = b2)
              | (PrimOp.BoolEqu, [V.NIL, V.BOOL _]) => V.BOOL false
              | (PrimOp.BoolEqu, [V.BOOL _, V.NIL]) => V.BOOL false
              | (PrimOp.BoolEqu, [V.NIL, V.NIL]) => V.BOOL true
              | (PrimOp.BoolNEq, [V.BOOL b1, V.BOOL b2]) => V.BOOL(b1 <> b2)
              | (PrimOp.BoolNEq, [V.NIL, V.BOOL _]) => V.BOOL true
              | (PrimOp.BoolNEq, [V.BOOL _, V.NIL]) => V.BOOL true
              | (PrimOp.BoolNEq, [V.NIL, V.NIL]) => V.BOOL false
              | (PrimOp.IntAdd, [V.INT i1, V.INT i2]) => mkInt(i1+i2)
              | (PrimOp.IntSub, [V.INT i1, V.INT i2]) => mkInt(i1-i2)
              | (PrimOp.IntMul, [V.INT i1, V.INT i2]) => mkInt(i1*i2)
              | (PrimOp.IntDiv, [V.INT _, V.INT 0]) => raise RuntimeError "division by zero"
              | (PrimOp.IntDiv, [V.INT i1, V.INT i2]) => mkInt(IntInf.quot(i1, i2))
              | (PrimOp.IntEqu, [V.INT i1, V.INT i2]) => V.BOOL(i1 = i2)
              | (PrimOp.IntEqu, [V.NIL, V.INT _]) => V.BOOL false
              | (PrimOp.IntEqu, [V.INT _, V.NIL]) => V.BOOL false
              | (PrimOp.IntEqu, [V.NIL, V.NIL]) => V.BOOL true
              | (PrimOp.IntNEq, [V.INT i1, V.INT i2]) => V.BOOL(i1 <> i2)
              | (PrimOp.IntNEq, [V.NIL, V.INT _]) => V.BOOL true
              | (PrimOp.IntNEq, [V.INT _, V.NIL]) => V.BOOL true
              | (PrimOp.IntNEq, [V.NIL, V.NIL]) => V.BOOL false
              | (PrimOp.IntLt, [V.INT i1, V.INT i2]) => V.BOOL(i1 < i2)
              | (PrimOp.IntLte, [V.INT i1, V.INT i2]) => V.BOOL(i1 <= i2)
              | (PrimOp.IntGt, [V.INT i1, V.INT i2]) => V.BOOL(i1 > i2)
              | (PrimOp.IntGte, [V.INT i1, V.INT i2]) => V.BOOL(i1 >= i2)
              | (PrimOp.StrEqu, [V.STR s1, V.STR s2]) => V.BOOL(s1 = s2)
              | (PrimOp.StrEqu, [V.NIL, V.STR _]) => V.BOOL false
              | (PrimOp.StrEqu, [V.STR _, V.NIL]) => V.BOOL false
              | (PrimOp.StrEqu, [V.NIL, V.NIL]) => V.BOOL true
              | (PrimOp.StrNEq, [V.STR s1, V.STR s2]) => V.BOOL(s1 <> s2)
              | (PrimOp.StrNEq, [V.NIL, V.STR _]) => V.BOOL true
              | (PrimOp.StrNEq, [V.STR _, V.NIL]) => V.BOOL true
              | (PrimOp.StrNEq, [V.NIL, V.NIL]) => V.BOOL false
              | (PrimOp.ObjEqu, [V.OBJ a1, V.OBJ a2]) => V.BOOL(a1 = a2)
              | (PrimOp.ObjEqu, [V.NIL, V.OBJ _]) => V.BOOL false
              | (PrimOp.ObjEqu, [V.OBJ _, V.NIL]) => V.BOOL false
              | (PrimOp.ObjEqu, [V.NIL, V.NIL]) => V.BOOL true
              | (PrimOp.ObjNEq, [V.OBJ a1, V.OBJ a2]) => V.BOOL(a1 <> a2)
              | (PrimOp.ObjNEq, [V.NIL, V.OBJ _]) => V.BOOL true
              | (PrimOp.ObjNEq, [V.OBJ _, V.NIL]) => V.BOOL true
              | (PrimOp.ObjNEq, [V.NIL, V.NIL]) => V.BOOL false
              | _ => raise RuntimeError(concat[
                    PrimOp.toString rator, "(", String.concatWithMap "," V.toString args, ")"
                  ])
            (* end case *)
          end

  (* evaluate an IR.value to a Value.t *)
    and evalValue (env, v) = (case v
           of IR.VarVal x => !(DE.lookupVar(env, x))
            | IR.FuncVal f => V.FUNC f
            | IR.ClassVal md => V.META md
            | IR.IndexVal tbl => V.INDEX tbl
            | IR.IntVal n => V.INT n
            | IR.BoolVal b => V.BOOL b
            | IR.StringVal s=> V.STR s
            | IR.NilVal => V.NIL
          (* end case *))

    and evalValues (env, vs : IR.value list) = let
          fun eval v = (case evalValue(env, v)
                 of V.UNDEF => expected(env, "value", V.UNDEF)
                  | v => v
                (* end case *))
          in
            List.map eval vs
          end

  end
