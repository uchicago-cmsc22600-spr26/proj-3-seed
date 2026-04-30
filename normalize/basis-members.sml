(* basis-members.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * These are SOIR implementations of the SooL Basis member functions.
 * Most of these are just wrappers around calls to runtime system functions.
 *)

structure BasisMembers : sig

  (* generate the IR functions for the basis member functions *)
    val genBasisFuns : Env.t -> SOIR.func_def list

  end = struct

    structure IR = SOIR

  (* add a SOIR local-variable declaration and pass the resulting value to `scope` *)
    fun withVar (name, ty, e, scope : IR.value -> IR.stm list) = let
          val x = SOIRVar.new (name, ty)
          in
            IR.LocalVarStm(x, SOME e) :: scope(IR.VarVal x)
          end

  (* add a SOIR apply statement and pass the resulting value to `scope` *)
    fun applyStm (name, ty, f, args, scope : IR.value -> IR.stm list) = let
          val x = SOIRVar.new (name, ty)
          in
            IR.ApplyStm(x, f, args) :: scope(IR.VarVal x)
          end

  (* member variable for accessing an object's metadata field *)
    val mdField = IR.MVAR{name = "_md", ty = IR.ClassTy, offset = 0}

  (* member variable for accessing a wrapper object's "value" field *)
    fun valField ty = IR.MVAR{name = "value", ty = ty, offset = 1}

  (* make an IR block *)
    fun mkBlock stms = IR.Block{lab = ProgPt.new(), stms = stms}

    fun ifThenElse (v, b1, b2) = IR.IfStm(v, b1, SOME b2)

  (* map an AST class and a member-function name to an IR member function *)
    fun lookupMFun (env, cls, f) =
          Env.lookupMFun (env, cls, valOf (Class.findFun (cls, Atom.atom f)))

    fun withValue (self, ty, scope) =
          withVar ("value", ty, IR.MemberVarExp(IR.VarVal self, valField ty), scope)

    val vv = IR.VarVal

  (***** boolI member functions *****)

  (* not
   *
   *    fun __Bool__.not (self : object) -> bool {
   *      var value = self.value
   *      var res = BoolNot(value)
   *      return res;
   *    }
   *)
    fun genBoolNot env = let
          val f = lookupMFun (env, Basis.boolCls, "not")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val body = mkBlock(
                withValue (self, IR.BoolTy, fn v =>
                withVar ("res", IR.BoolTy, IR.PrimExp(PrimOp.BoolNot, [v]), fn res =>
                  [IR.ReturnStm(SOME res)])))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* toString
   *
   *    fun __Bool__.toString (self : object) -> string {
   *      var value = self.value
   *      if value {
   *        return "true"
   *      } else {
   *        return "false"
   *      }
   *    }
   *)
(*
    fun genBoolToString env = let
          val f = lookupMFun (env, Basis.boolCls, "toString")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val body = mkBlock(
                withVar ("value", IR.BoolTy, IR.MemberVarExp(vv self, valField IR.BoolTy), fn v =>
                  [ifThenElse(v,
                    mkBlock[IR.ReturnStm(SOME(IR.StringVal "true"))],
                    mkBlock[IR.ReturnStm(SOME(IR.StringVal "false"))])]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end
*)

  (* An alternate implementation of boolI.toString that does not use conditional.  We use this
   * implementation to allow straight-line code generation to be tested while conditionals
   * are unimplemented.
   *)
  (* toString
   *
   *    fun __Bool__.toString (self : object) -> string {
   *      var value = self.value
   *      var res = _sool_bool_to_string (value)
   *      return res
   *    }
   *)
    fun genBoolToString env = let
          val f = lookupMFun (env, Basis.boolCls, "toString")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val res = SOIRVar.new ("res", IR.StringTy)
          val body = mkBlock(
                withValue (self, IR.BoolTy, fn v =>
                  [IR.ApplyStm(res, IR.FuncVal SOIRBasis.boolToStringFunc, [v]),
                  IR.ReturnStm(SOME(vv res))
                ]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (***** intI member function *****)

  (* char
   *
   *    fun __Int__.char (self : object) -> string {
   *      var value = self.value
   *      var res = _sool_int_to_char (value)
   *      return res
   *    }
   *)
    fun genIntChar env = let
          val f = lookupMFun (env, Basis.intCls, "char")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val res = SOIRVar.new ("res", IR.StringTy)
          val body = mkBlock(
                withValue (self, IR.IntTy, fn v =>
                  [IR.ApplyStm(res, IR.FuncVal SOIRBasis.intCharFunc, [v]),
                  IR.ReturnStm(SOME(vv res))
                ]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* toString
   *
   *    fun __Int__.toString (self : object) -> string {
   *      var value = self.value
   *      var res = _sool_int_to_string (value)
   *      return res
   *    }
   *)
    fun genIntToString env = let
          val f = lookupMFun (env, Basis.intCls, "toString")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val res = SOIRVar.new ("res", IR.StringTy)
          val body = mkBlock(
                withValue (self, IR.IntTy, fn v =>
                  [IR.ApplyStm(res, IR.FuncVal SOIRBasis.intToStringFunc, [v]),
                  IR.ReturnStm(SOME(vv res))
                ]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (***** stringI member function *****)

  (* charAt
   *
   *    fun __String__.charAt (self : object, ix : int) -> int {
   *      var value = self.value
   *      var res = _sool_char_at (value)
   *      return res
   *    }
   *)
    fun genStringCharAt env = let
          val f = lookupMFun (env, Basis.stringCls, "charAt")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val ix = SOIRVar.new ("ix", IR.IntTy)
          val params = [self, ix]
          val res = SOIRVar.new ("res", IR.IntTy)
          val body = mkBlock(
                withValue (self, IR.StringTy, fn v =>
                  [IR.ApplyStm(res, IR.FuncVal SOIRBasis.stringCharAtFunc, [v, vv ix]),
                  IR.ReturnStm(SOME(vv res))
                ]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* length
   *
   *    fun __String__.length (self : object) -> int {
   *      var value = self.value
   *      var res = _sool_string_length (value)
   *      return res
   *    }
   *)
    fun genStringLength env = let
          val f = lookupMFun (env, Basis.stringCls, "length")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val res = SOIRVar.new ("res", IR.IntTy)
          val body = mkBlock(
                withValue (self, IR.StringTy, fn v =>
                  [IR.ApplyStm(res, IR.FuncVal SOIRBasis.stringLengthFunc, [v]),
                  IR.ReturnStm(SOME(vv res))
                ]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* substring
   *
   *    fun __String__.substring (self : object, i : int, n : int) -> string {
   *      var value = self.value
   *      var res = _sool_substring (value, i, n)
   *      return res
   *    }
   *)
    fun genStringSubstring env = let
          val f = lookupMFun (env, Basis.stringCls, "substring")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val i = SOIRVar.new ("i", IR.IntTy)
          val n = SOIRVar.new ("n", IR.IntTy)
          val params = [self, i, n]
          val res = SOIRVar.new ("res", IR.StringTy)
          val body = mkBlock(
                withValue (self, IR.StringTy, fn v =>
                  [IR.ApplyStm(res, IR.FuncVal SOIRBasis.stringSubstringFunc, [v, vv i, vv n]),
                  IR.ReturnStm(SOME(vv res))
                ]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* toInt
   *
   *    fun __String__.toInt (self : object) -> int? {
   *      var value = self.value
   *      var res = _sool_string_to_int (value)
   *      return res
   *    }
   *)
    fun genStringToInt env = let
          val f = lookupMFun (env, Basis.stringCls, "toInt")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val res = SOIRVar.new ("res", IR.OptionTy IR.IntTy)
          val body = mkBlock(
                withValue (self, IR.StringTy, fn v =>
                  [IR.ApplyStm(res, IR.FuncVal SOIRBasis.stringToIntFunc, [v]),
                  IR.ReturnStm(SOME(vv res))
                ]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* toString
   *
   *    fun __String__.toString (self : object) -> string {
   *      var value = self.value
   *      return value
   *    }
   *)
    fun genStringToString env = let
          val f = lookupMFun (env, Basis.stringCls, "toString")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val res = SOIRVar.new ("res", IR.StringTy)
          val body = mkBlock(withValue (self, IR.StringTy, fn v => [IR.ReturnStm(SOME v)]))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (***** systemI member function *****)

  (* exit
   *
   *    fun __System__.exit (self : object) -> void {
   *      _sool_exit();
   *    }
   *)
    fun genSystemExit env = let
          val f = lookupMFun (env, Basis.systemCls, "exit")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val params = [self]
          val body = mkBlock[
                  IR.CallStm(IR.FuncVal SOIRBasis.exitFunc, []),
                  IR.ReturnStm NONE
                ]
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* fail
   *
   *    fun __System__.fail (self : object, msg : string) -> void {
   *      _sool_fail(msg);
   *    }
   *)
    fun genSystemFail env = let
          val f = lookupMFun (env, Basis.systemCls, "fail")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val msg = SOIRVar.new ("msg", IR.StringTy)
          val params = [self, msg]
          val body = mkBlock[
                  IR.CallStm(IR.FuncVal SOIRBasis.failFunc, [vv msg]),
                  IR.ReturnStm NONE
                ]
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* input
   *
   *    fun __System__.input (self : object) -> string? {
   *      var res = _sool_input()
   *      return res
   *    }
   *)
    fun genSystemInput env = let
          val f = lookupMFun (env, Basis.systemCls, "input")
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val res = SOIRVar.new ("res", IR.OptionTy IR.StringTy)
          val params = [self]
          val body = mkBlock[
                  IR.ApplyStm(res, IR.FuncVal SOIRBasis.inputFunc, []),
                  IR.ReturnStm(SOME(vv res))
                ]
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

  (* print
   *
   *    fun __System__.print (self : object, arg : <object, index_table>) -> void {
   *      var obj = #1(arg)
   *      var index = #2(arg)
   *      var md = obj._md;
   *      var ix = index::toString (@0)
   *      var toString = md[ix]
   *      var s = toString(obj);
   *      _sool_print (s);
   *      return;
   *    }
   *)
    fun genSystemPrint env = let
          val f = Env.lookupMFun (env, Basis.systemCls, Basis.systemPrintFun)
          val self = SOIRVar.new ("self", IR.ObjectTy)
          val arg = SOIRVar.new ("arg", IR.TupleTy[IR.ObjectTy, IR.IndexTy])
          val params = [self, arg]
          val fnTy = IR.FuncTy([IR.ObjectTy], IR.StringTy)
          val body = mkBlock(
                withVar ("obj", IR.ObjectTy, IR.TupleSel(1, vv arg), fn obj =>
                withVar ("index", IR.IndexTy, IR.TupleSel(2, vv arg), fn index =>
                withVar ("md", IR.ClassTy, IR.MemberVarExp(obj, mdField), fn md =>
                withVar ("ix", IR.IntTy, IR.MetaSel(index, ("toString", 0)), fn ix =>
                withVar ("toString", fnTy, IR.IndexSel(md, ix), fn toString =>
                applyStm ("s", IR.StringTy, toString, [obj], fn s => [
                    IR.CallStm(IR.FuncVal SOIRBasis.printFunc, [s]),
                    IR.ReturnStm NONE
                  ])))))))
          in
            IR.FuncDef{f = f, params = params, body = body}
          end

    fun genBasisFuns env = List.map (fn gen => gen env) [
            genBoolNot,
            genBoolToString,
            genIntChar,
            genIntToString,
            genStringCharAt,
            genStringLength,
            genStringSubstring,
            genStringToInt,
            genStringToString,
            genSystemExit,
            genSystemFail,
            genSystemInput,
            genSystemPrint
          ]

  end
