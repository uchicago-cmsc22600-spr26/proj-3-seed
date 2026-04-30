(* soir-util.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure SOIRUtil : sig

  (* convert statements, expressions, and values to strings *)
    val stmToString : SOIR.stm -> string
    val expToString : SOIR.exp -> string
    val valToString : SOIR.value -> string

  end = struct

    structure IR = SOIR

    local
      fun stm2s (IR.LabelStm(lab, stm)) =
            concat [ProgPt.toString lab, ": ", stm2s stm]
        | stm2s (IR.LocalVarStm(x, NONE)) = concat["var ", SOIRVar.nameOf x, ";"]
        | stm2s (IR.LocalVarStm(x, SOME e)) =
            concat["var ", SOIRVar.nameOf x, " := ", exp2s e, ";"]
        | stm2s (IR.LocalAssignStm(x, e)) =
            concat[SOIRVar.nameOf x, " := ", exp2s e, ";"]
        | stm2s (IR.AssignStm(v, IR.MVAR{name, ...}, e)) =
            concat[val2s v, ".", name, " := ", exp2s e, ";"]
        | stm2s (IR.NewStm(x, IR.CLASS{tyc, ...})) =
            concat["var ", SOIRVar.nameOf x, " := new " ^ Class.nameOf tyc]
        | stm2s (IR.TupleStm(x, args)) =
            concat["var ", SOIRVar.nameOf x, " := <", String.concatWithMap "," val2s args, ">"]
        | stm2s (IR.ApplyStm(x, f, args)) =
            concat["var ", SOIRVar.nameOf x, " := ", app2s (val2s f, args, "")]
        | stm2s (IR.CallStm(f, args)) = app2s (val2s f, args, ";")
        | stm2s (IR.IfStm(v, _, NONE)) = concat["if ", val2s v, " then { ... }"]
        | stm2s (IR.IfStm(v, _, SOME _)) =
            concat["if ", val2s v, " then { ... } else { ... }"]
        | stm2s (IR.LoopStm _) = "loop { ... }"
        | stm2s (IR.ExitIfStm v) = concat["exit_if ", val2s v, ";"]
        | stm2s (IR.ReturnStm NONE) = "return;"
        | stm2s (IR.ReturnStm(SOME v)) = concat["return ", val2s v, ";"]

      and exp2s (IR.ValExp v) = val2s v
        | exp2s (IR.MemberVarExp(v, IR.MVAR{name, ...})) = concat[val2s v, ".", name]
        | exp2s (IR.MetaSel(v, (name, offset))) =
            concat[val2s v, "$", name, "(@", Int.toString offset, ")"]
        | exp2s (IR.IndexSel(v, ix)) = concat[val2s v, "[", val2s ix, "]"]
        | exp2s (IR.PrimExp(rator, args)) = app2s (PrimOp.toString rator, args, "")
        | exp2s (IR.TupleSel(i, v)) = concat["#", Int.toString i, "(", val2s v, ")"]

      and val2s (IR.VarVal x) = SOIRVar.nameOf x
        | val2s (IR.FuncVal f) = SOIRFunc.nameOf f
        | val2s (IR.ClassVal(IR.CLASS{tyc, ...})) = Class.nameOf tyc
        | val2s (IR.IndexVal(IR.INDEX{name, ...})) = name
        | val2s (IR.IntVal n) = Format.format "%d" [Format.LINT n]
        | val2s (IR.BoolVal b) = Bool.toString b
        | val2s (IR.StringVal s) = concat["\"", String.toString s, "\""]
        | val2s (IR.NilVal) = "nil"

      and app2s (f, args, semi) =
            concat[f, "(", String.concatWithMap "," val2s args, ")", semi]
    in
    val stmToString = stm2s
    val expToString = exp2s
    val valToString = val2s
    end (* local *)

  end

