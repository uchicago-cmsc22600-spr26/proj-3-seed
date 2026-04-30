(* dynamic-env.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure DynamicEnv : sig

    type t

    type controls = {
        trace : bool,                   (* if true, we generate a trace of the execution *)
        stepLimit : int,                (* limit on number of statements executed *)
        depthLimit : int                (* limit on call-stack depth *)
      }

    val new : controls * SOIR.program -> t

    datatype function
      = UserFn of SOIR.func_def
      | PrimFn of Value.t list -> Value.t

  (* exception used to signal a runtime error *)
    exception RuntimeError of string

  (* dump the environment and raise a RuntimeError exception *)
    val runtimeError : t * string -> 'a

    val lookupFunc : t * SOIR.func -> function

    val bindParams : t * SOIR.var list * Value.t list -> t

    val bindVar : t * SOIR.var * Value.t -> t

    val lookupVar : t * SOIR.var -> Value.t ref

    val traceStm : t * SOIR.stm -> unit

    val traceCall : t * SOIR.func * Value.t list -> unit

    val traceReturn : t -> unit

  end = struct

    structure IR = SOIR
    structure V = Value
    structure FTbl = SOIRFunc.Tbl
    structure VMap = VarMap

    type varmap = VarMap.t

    type controls = {
        trace : bool,                   (* if true, we generate a trace of the execution *)
        stepLimit : int,                (* limit on number of statements executed *)
        depthLimit : int                (* limit on call-stack depth *)
      }

    datatype t = Env of {
        vEnv : varmap,                          (* local variable environment *)
        gInfo : global_info
      }

    and global_info = GInfo of {
        ctls : controls,                        (* controls set at program start *)
        globalVEnv : varmap,                    (* global env binds system variable *)
        funcs : IR.func -> function option,     (* mapping from IR functions to their definitions *)
        step : int ref,                         (* statement count *)
        depth : int ref,                        (* call-depth count *)
        callStk : call list ref,
        lastStm : IR.stm ref                    (* the most recent statement that was executed *)
      }

    and function
      = UserFn of SOIR.func_def
      | PrimFn of Value.t list -> Value.t

    withtype call = IR.func * Value.t list

  (* exception used to signal a runtime error *)
    exception RuntimeError of string

    fun runtimeError (Env{vEnv, ...}, msg) = (
          VMap.dump (TextIO.stdOut, vEnv);
          raise RuntimeError msg)

  (* the initial variable environment binds the global system variable *)
    fun new (ctls, IR.PROGRAM{classes, indices, functions, main}) = let
          fun findCls cls = let
                fun isCls (IR.CLASS{tyc, ...}) = Class.same(cls, tyc)
                in
                  case List.find isCls classes
                   of SOME md => md
                    | NONE => Runtime.failure("no metadata for class "^Class.nameOf cls)
                  (* end case *)
                end
          fun findFunc (cls, f) = let
                val f = concat[Class.nameOf cls, ".", f]
                fun isF (IR.FuncItem f') = (SOIRFunc.nameOf f' = f)
                  | isF _ = false
                val IR.CLASS{metadata, ...} = findCls cls
                in
                  case List.find isF metadata
                   of SOME(IR.FuncItem f') => f'
                    | _ => Runtime.failure("no function for builtin " ^ f)
                  (* end case *)
                end
          val funcs = let
                val fTbl = FTbl.mkTable (List.length functions+16, Fail "func-table")
                fun ins (fd as IR.FuncDef{f, ...}) = FTbl.insert fTbl (f, UserFn fd)
                in
                  List.app ins (main::functions);
                  List.app (FTbl.insert fTbl) [
                      (findFunc(Basis.boolCls, "toString"), PrimFn Runtime.bool_toString),
                      (findFunc(Basis.boolCls, "not"), PrimFn Runtime.bool_not),
                      (findFunc(Basis.intCls, "toString"), PrimFn Runtime.int_toString),
                      (findFunc(Basis.intCls, "char"), PrimFn Runtime.int_char),
                      (findFunc(Basis.stringCls, "toString"), PrimFn Runtime.string_toString),
                      (findFunc(Basis.stringCls, "length"), PrimFn Runtime.string_length),
                      (findFunc(Basis.stringCls, "substring"), PrimFn Runtime.string_substring),
                      (findFunc(Basis.stringCls, "charAt"), PrimFn Runtime.string_charAt),
                      (findFunc(Basis.stringCls, "toInt"), PrimFn Runtime.string_toInt),
                      (* NOTE: "system.print" is generated *)
                      (findFunc(Basis.systemCls, "input"), PrimFn Runtime.system_input),
                      (findFunc(Basis.systemCls, "exit"), PrimFn Runtime.system_exit),
                      (findFunc(Basis.systemCls, "fail"), PrimFn Runtime.system_fail),
                      (SOIRBasis.failFunc, PrimFn Runtime.failFunc),
                      (SOIRBasis.printFunc, PrimFn Runtime.printFunc),
                      (SOIRBasis.stringCatFunc, PrimFn Runtime.stringCat),
                      (SOIRBasis.instanceOfFunc, PrimFn Runtime.instanceOf)
                    ];
                  FTbl.find fTbl
                end
        (* the global environment maps the system variable to its runtime representation *)
          val globalVEnv = VMap.singleton(
                SOIRBasis.systemVar,
                Runtime.systemVar(findCls Basis.systemCls))
          val gInfo = GInfo{
                  ctls = ctls,
                  globalVEnv = globalVEnv,
                  funcs = funcs,
                  step = ref 0,
                  depth = ref 0,
                  callStk = ref[],
                  lastStm = ref(IR.ReturnStm NONE)
                }
          in
            Env{vEnv = globalVEnv, gInfo = gInfo}
          end

    fun lookupFunc (Env{gInfo=GInfo{funcs, ...}, ...}, f) = (case funcs f
           of SOME fd => fd
            | NONE => raise RuntimeError(concat[
                  "undefined function \"", SOIRFunc.nameOf f, "\""
                ])
          (* end case *))

    fun bindParams (Env{gInfo as GInfo{globalVEnv, ...}, ...}, params, args) = let
          fun bind ([], [], vEnv) = Env{vEnv = vEnv, gInfo = gInfo}
            | bind (x::xs, v::vs, vEnv) = bind (xs, vs, VMap.insert(vEnv, x, v))
            | bind ([], _, _) = raise RuntimeError "too many arguments to function call"
            | bind (_, [], _) = raise RuntimeError "too few arguments to function call"
          in
            bind (params, args, globalVEnv)
          end

    fun bindVar (Env{vEnv, gInfo}, x, v) =
          Env{vEnv = VMap.insert(vEnv, x, v), gInfo = gInfo}

    fun lookupVar (env as Env{vEnv, ...}, x) = (case VMap.find (vEnv, x)
           of SOME r => r
            | NONE => runtimeError (env, concat[
                  "undefined local variable \"", SOIRVar.nameOf x, "\""
                ])
          (* end case *))

    fun traceStm (Env{gInfo=GInfo{ctls, step, lastStm, ...}, ...}, stm) = (
          lastStm := stm;
          step := !step + 1;
          if #trace ctls
            then print(concat[
                StringCvt.padLeft #" " 4 (Int.toString (!step)), ": ",
                Util.stmToString stm, "\n"
              ])
            else ();
          if (!step >= #stepLimit ctls)
            then raise RuntimeError "step limit exceeded"
            else ())

    fun traceCall (Env{gInfo=GInfo{ctls, depth, callStk, ...}, ...}, f, args) = (
          if #trace ctls
            then print(concat[
                "      ENTER ", SOIRFunc.nameOf f, " (",
                String.concatWithMap "," V.toString args, ")\n"
              ])
            else ();
          callStk := (f, args) :: !callStk;
          depth := !depth + 1;
          if (!depth >= #depthLimit ctls)
            then raise RuntimeError "call-depth limit exceeded"
            else ())

    fun traceReturn (Env{gInfo=GInfo{ctls, depth, callStk, ...}, ...}) = (
          case !callStk
           of (f, args)::r => (
               if #trace ctls
                 then print(concat[
                     "      EXIT ", SOIRFunc.nameOf f, "\n"
                   ])
                 else ();
                callStk := r;
                depth := !depth - 1)
            | _ => raise RuntimeError "unmatched return statement"
          (* end case *))

  end

