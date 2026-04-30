(* util.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Util : sig

  (* convert SOIR statements to strings *)
    val stmToString : SOIR.stm -> string

  end = struct

    structure IR = SOIR

    fun stmToString (IR.LabelStm _) = raise Fail "unexpected LabelStm"
      | stmToString (IR.LocalVarStm(x, NONE)) = concat["var ", SOIRVar.nameOf x, ";"]
      | stmToString (IR.LocalVarStm(x, SOME e)) =
          concat["var ", SOIRVar.nameOf x, " := ", expToString e, ";"]
      | stmToString (IR.LocalAssignStm(x, e)) =
          concat[SOIRVar.nameOf x, " := ", expToString e, ";"]
      | stmToString (IR.AssignStm(v, IR.MVAR{name, ...}, e)) =
          concat[valToString v, ".", name, " := ", expToString e, ";"]
      | stmToString (IR.NewStm(x, IR.CLASS{tyc, ...})) =
          concat["var ", SOIRVar.nameOf x, " := new " ^ Class.nameOf tyc]
      | stmToString (IR.TupleStm(x, args)) =
          concat["var ", SOIRVar.nameOf x, " := <", String.concatWithMap "," valToString args, ">"]
      | stmToString (IR.ApplyStm(x, f, args)) =
          concat["var ", SOIRVar.nameOf x, " := ", appToStr (valToString f, args, "")]
      | stmToString (IR.CallStm(f, args)) = appToStr (valToString f, args, ";")
      | stmToString (IR.IfStm(v, _, NONE)) = concat["if ", valToString v, " then { ... }"]
      | stmToString (IR.IfStm(v, _, SOME _)) =
          concat["if ", valToString v, " then { ... } else { ... }"]
      | stmToString (IR.LoopStm _) = "loop { ... }"
      | stmToString (IR.ExitIfStm v) = concat["exit_if ", valToString v, ";"]
      | stmToString (IR.ReturnStm NONE) = "return;"
      | stmToString (IR.ReturnStm(SOME v)) = concat["return ", valToString v, ";"]

    and expToString (IR.ValExp v) = valToString v
      | expToString (IR.MemberVarExp(v, IR.MVAR{name, ...})) = concat[valToString v, ".", name]
      | expToString (IR.MetaSel(v, (name, offset))) =
          concat[valToString v, "::", name, "(@", Int.toString offset, ")"]
      | expToString (IR.IndexSel(v, ix)) = concat[valToString v, "[", valToString ix, "]"]
      | expToString (IR.PrimExp(rator, args)) = appToStr (PrimOp.toString rator, args, "")
      | expToString (IR.TupleSel(i, v)) = concat["#", Int.toString i, "(", valToString v, ")"]

    and valToString (IR.VarVal x) = SOIRVar.nameOf x
      | valToString (IR.FuncVal f) = SOIRFunc.nameOf f
      | valToString (IR.ClassVal(IR.CLASS{tyc, ...})) = Class.nameOf tyc
      | valToString (IR.IndexVal(IR.INDEX{name, ...})) = name
      | valToString (IR.IntVal n) = Format.format "%d" [Format.LINT n]
      | valToString (IR.BoolVal b) = Bool.toString b
      | valToString (IR.StringVal s) = concat["\"", String.toString s, "\""]
      | valToString (IR.NilVal) = "nil"

    and appToStr (f, args, semi) =
          concat[f, "(", String.concatWithMap "," valToString args, ")", semi]

  end

