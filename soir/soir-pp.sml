(* soir-pp.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Pretty printer for SOIR.
 *)

structure SOIRPP : sig

    val output : TextIO.outstream * string * SOIR.program -> unit

  end = struct

    structure IR = SOIR
    structure PP = TextIOPP

    val indent0 = PP.Abs 0
    val indent2 = PP.Abs 2

    fun ppList ppFn (left, sep, right) (ppStrm, xs) = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          fun pp [] = string right
            | pp [x] = (ppFn x; string right)
            | pp (x::xs) = (ppFn x; string sep; sp(); pp xs)
          in
            string left; pp xs
          end

  (* table indices *)
    fun indexToString i = concat["[", StringCvt.padLeft #" " 3 (Int.toString i), "]"]

    fun classToString (IR.CLASS{tyc, ...}) = Class.nameOf tyc

    fun ppClass ppStrm (IR.CLASS{tyc, super, init, vars, metadata}) = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          fun ppVar (IR.MVAR{name, ty, offset}) = (
                nl();
                PP.openHBox ppStrm;
                  string(indexToString offset); sp();
                  string "var"; sp(); string name; sp(); string ":"; sp();
                  string(SOIRType.toString ty); sp();
                PP.closeBox ppStrm)
          (* print metadata items; the indices start at 2 (after the size and
           * super-class slots)
           *)
          fun ppItem (i, item) = (
                nl();
                PP.openHBox ppStrm;
                  string(indexToString(i+2)); sp();
                  case item
                   of IR.FuncItem(IR.FUNC{name, ...}) => string name
                    | IR.IndexItem(IR.INDEX{name, ...}) => string name
                  (* end case *);
                PP.closeBox ppStrm)
          in
            PP.openVBox ppStrm indent2;
              PP.openHBox ppStrm;
                string "class"; sp(); string(Class.nameOf tyc); sp();
                case super
                 of SOME cls' => (string "extends"; sp(); string(classToString cls'))
                  | NONE => ()
                (* end case *);
                string "{";
              PP.closeBox ppStrm;
              List.app ppVar vars;
              nl ();
              PP.openVBox ppStrm indent2;
                PP.openHBox ppStrm;
                  string "metadata"; sp(); string "{";
                PP.closeBox ppStrm;
                nl();
                (* the size field *)
                PP.openHBox ppStrm;
                  string (indexToString 0); sp(); string(Int.toString(length vars + 1));
                PP.closeBox ppStrm;
                nl();
                (* the super-class field *)
                PP.openHBox ppStrm;
                  string (indexToString 1); sp();
                  case super
                   of NONE => string "nil"
                    | SOME cls' => string(classToString cls');
                PP.closeBox ppStrm;
                List.appi ppItem metadata;
              PP.closeBox ppStrm;
              nl(); string "}";
            PP.closeBox ppStrm;
            nl ();
            string "}";
            nl ()
          end

    fun ppIndex ppStrm (IR.INDEX{name, items, ...}) = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          fun ppItem (i, (name, offset)) = (
                nl();
                PP.openHBox ppStrm;
                  string(indexToString i); sp(); string name; string ":"; sp();
                  string "@"; string(Int.toString offset);
                PP.closeBox ppStrm)
          in
            PP.openVBox ppStrm indent2;
              PP.openHBox ppStrm;
                string "index_table"; sp(); string name; sp(); string "{";
              PP.closeBox ppStrm;
              List.appi ppItem (!items);
            PP.closeBox ppStrm;
            nl ();
            string "}";
            nl ()
          end

    fun ppFuncDef ppStrm (IR.FuncDef{f, params, body}) = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
        (* print contents inside an H-Box *)
          fun inHBox pp = (PP.openHBox ppStrm; pp(); PP.closeBox ppStrm)
        (* print a value *)
          fun ppVal v = (case v
               of IR.VarVal x => string(SOIRVar.nameOf x)
                | IR.FuncVal f => string(SOIRFunc.nameOf f)
                | IR.ClassVal(IR.CLASS{tyc, ...}) => string(Class.nameOf tyc)
                | IR.IndexVal(IR.INDEX{name, ...}) => string name
                | IR.IntVal n => if (n < 0)
                    then string("-" ^ IntInf.toString(~n))
                    else string(IntInf.toString n)
                | IR.BoolVal b => string(Bool.toString b)
                | IR.StringVal s => string(concat["\"", String.toString s, "\""])
                | IR.NilVal => string "nil"
              (* end case *))
        (* print a variable with its type *)
          fun ppVarBind x = (
                string(SOIRVar.nameOf x); sp(); string ":"; sp();
                string(SOIRType.toString(SOIRVar.typeOf x)))
        (* print the 'var x : ty =' part of a binding *)
          fun ppLHS x = (string "var"; sp(); ppVarBind x; sp(); string "="; sp())
        (* print a block *)
          fun ppBlk (IR.Block{stms=[], ...}) = string "{ }"
            | ppBlk (IR.Block{lab, stms}) = (
                PP.openVBox ppStrm indent0;
                  string "{";
                  PP.openVBox ppStrm indent2;
                    List.app ppStm stms;
                  PP.closeBox ppStrm;
                  nl();
                  string "}";
                PP.closeBox ppStrm)
          and ppStm stm = (
                nl();
                case stm
                 of IR.LabelStm(lab, stm) => inHBox (fn () => (
                      string(ProgPt.toString lab ^ ":"); sp(); ppStm stm))
                  | IR.LocalVarStm(x, NONE) => inHBox (fn () => (
                      string "var"; sp(); ppVarBind x; string ";"))
                  | IR.LocalVarStm(x, SOME e) => inHBox (fn () => (
                      ppLHS x; ppExp e; string ";"))
                  | IR.LocalAssignStm(x, e) => inHBox (fn () => (
                      string(SOIRVar.nameOf x); sp(); string ":="; sp(); ppExp e; string ";"))
                  | IR.AssignStm(x, SOIR.MVAR{name, ...}, e) => inHBox (fn () => (
                      ppVal x; string "."; string name; sp();
                      string ":="; sp(); ppExp e; string ";"))
                  | IR.NewStm(x, IR.CLASS{tyc, ...}) => inHBox (fn () => (
                      ppLHS x; string "new"; sp(); string(Class.nameOf tyc)))
                  | IR.TupleStm(x, args) => inHBox (fn () => (
                      ppLHS x; ppList ppVal ("<", ",", ">") (ppStrm, args)))
                  | IR.ApplyStm(x, f, args) => inHBox (fn () => (
                      ppLHS x; ppVal f; sp(); ppList ppVal ("(", ",", ")") (ppStrm, args)))
                  | IR.CallStm(f, args) => inHBox (fn () => (
                      ppVal f; sp(); ppList ppVal ("(", ",", ")") (ppStrm, args)))
                  | IR.IfStm(v, thenBlk, optBlk) => (
                      PP.openVBox ppStrm indent0;
                        inHBox (fn () => (string "if"; sp(); ppVal v; sp(); string "then"; sp()));
                        ppBlk thenBlk;
                        case optBlk
                         of NONE => ()
                          | SOME blk => (
                              nl(); inHBox (fn () => (string "else"; sp())); ppBlk blk)
                        (* end case *);
                      PP.closeBox ppStrm)
                  | IR.LoopStm blk => (
                      inHBox (fn () => (string "loop"; sp()));
                      ppBlk blk)
                  | IR.ExitIfStm v => inHBox (fn () => (
                      string "exit_if"; sp(); ppVal v; string ";"))
                  | IR.ReturnStm NONE => string "return;"
                  | IR.ReturnStm(SOME v) => inHBox (fn () => (
                      string "return"; sp(); ppVal v; string ";"))
                (* end case *))
          and ppExp e = (
                PP.openHBox ppStrm;
                  case e
                   of IR.ValExp v => ppVal v
                    | IR.MemberVarExp(v, SOIR.MVAR{name, ...}) => (
                        ppVal v; string "."; string name)
                    | IR.MetaSel(v1, (name, offset)) => (
                        ppVal v1; string "::"; string name;
                        sp(); string "@"; string(Int.toString offset))
                    | IR.IndexSel(v1, v2) => (ppVal v1; string "["; ppVal v2; string "]")
                    | IR.PrimExp(p, args) => (
                        string(PrimOp.toString p); sp();
                        ppList ppVal ("(", ",", ")") (ppStrm, args))
                    | IR.TupleSel(i, v) => (
                        string "#"; string(Int.toString i); string "("; ppVal v; string ")")
                  (* end case *);
                PP.closeBox ppStrm)
          in
            PP.openVBox ppStrm indent0;
              inHBox (fn () => (
                string "fun"; sp(); string(SOIRFunc.nameOf f); sp();
                ppList ppVarBind ("(", ",", ")") (ppStrm, params);
                sp(); string "->"; sp(); string(SOIRType.toString(SOIRFunc.returnTypeOf f));
                sp()));
              ppBlk body;
              nl();
            PP.closeBox ppStrm
          end

    fun output (outS, message, SOIR.PROGRAM{classes, indices, functions, main}) = let
          val ppStrm = PP.openOut {dst = outS, wid = 100}
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          in
            PP.openVBox ppStrm indent0;
              string (concat ["/* SOIR: ", message, " */"]); nl();
              List.app (ppClass ppStrm) classes;
              List.app (ppIndex ppStrm) indices;
              List.app (ppFuncDef ppStrm) functions;
              ppFuncDef ppStrm main;
              string "/* Program end */"; nl();
            PP.closeBox ppStrm;
            PP.closeStream ppStrm
          end

  end
