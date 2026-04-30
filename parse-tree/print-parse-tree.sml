(* print-parse-tree.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure PrintParseTree : sig

    type strm

  (* create a new output stream *)
    val new : {
            outS : TextIO.outstream,    (* output stream to print to *)
            errS : Error.err_stream,    (* error stream from parse; used to report locations *)
            showMarks : bool            (* if true, node position information will be included *)
          } -> strm

  (* print functions for parse-tree non-terminals *)
    val program : strm * ParseTree.program -> unit
    val dcl     : strm * ParseTree.dcl -> unit
    val param   : strm * ParseTree.param -> unit
    val typ     : strm * ParseTree.typ -> unit
    val varDcl  : strm * ParseTree.var_dcl -> unit
    val methDcl : strm * ParseTree.meth_dcl -> unit
    val methSpc : strm * ParseTree.meth_spc -> unit
    val exp     : strm * ParseTree.exp -> unit
    val member  : strm * ParseTree.member -> unit
    val blk     : strm * ParseTree.blk -> unit
    val stm     : strm * ParseTree.stm -> unit

  (* `dump pr tr` prints the parse-tree `tr` to standard output using the
   * printing function pr
   *)
    val dump : ((strm * 'a) -> unit) -> 'a -> unit

  end = struct

    structure PT = ParseTree
    structure E = Error

    datatype strm = S of {
        indent : int,                   (* current indentation level *)
        span : E.span,                  (* current source-file span *)
        info : strm_info
      }

    and strm_info = Info of {
        mark : bool,                    (* if true, print mark info *)
        errS : Error.err_stream,        (* for interpreting spans *)
        outS : TextIO.outstream         (* output I/O stream to print to *)
      }

  (* create a new output stream *)
    fun new {errS, outS, showMarks} = S{
            indent = 0, span = (0, 0),
            info = Info{mark = showMarks, errS = errS, outS = outS}
          }

  (* print text *)
    fun pr (S{info=Info{outS, ...}, ...}, txt) = TextIO.output(outS, txt)

  (* print a newline *)
    fun nl (S{info=Info{outS, ...}, ...}) = TextIO.output1 (outS, #"\n")

  (* print whitespace to indent the text *)
    fun prIndent (S{indent=n, info=Info{outS, ...}, ...}) = let
          fun lp 0 = ()
            | lp i = (TextIO.output(outS, "  "); lp(i-1))
          in
            lp n
          end

  (* increment indentation level *)
    fun inc (S{indent, span, info}) = S{indent=indent+1, span=span, info=info}

    fun nest strm f = f (inc strm)

  (* print location information *)
    fun prLoc (S{span, info, ...}) = (case info
           of Info{mark=true, errS, outS, ...} =>
                TextIO.output (outS, Error.locToString (Error.location (errS, span)))
            | _ => ()
          (* end case *))

  (* set the span of a strm *)
    fun setSpan (S{indent, info, ...}, span) = S{span=span, indent=indent, info=info}

  (* update the current span *)
    fun mark (strm, {span, tree}) = (setSpan(strm, span), tree)

  (* print a line consisting of a string *)
    fun prStr (strm, s) = (prIndent strm; pr (strm, s); nl strm)

  (* print a line consisting of a marked identifier *)
    fun prId (strm, {span, tree}) = prStr (setSpan(strm, span), Atom.toString tree)

    fun prNode (strm, name) = (prIndent strm; pr (strm, name); prLoc strm; nl strm)
    fun prLeaf (strm, name, arg) = (
          prIndent strm; pr (strm, concat[name, "(", arg, ")"]); prLoc strm; nl strm)

  (* print a list of items enclosed in "[" ... "]" *)
    fun prList prItem (strm, []) = (prIndent strm; pr (strm, "[ ]\n"))
      | prList prItem (strm, items) = (
          prIndent strm; pr (strm, "[\n");
          nest strm
            (fn strm' => List.app (fn item => prItem (strm', item)) items);
          prIndent strm; pr (strm, "]\n"))

  (* print an optional item *)
    fun prOpt prItem (strm, NONE) = prStr (strm, "<none>")
      | prOpt prItem (strm, SOME item) = prItem (strm, item)

  (* dump a parse-tree term to the standard output for debugging purposes *)
    fun dump printFn = let
          val strm = new{outS = TextIO.stdOut, errS = E.mkErrStream "bogus", showMarks = false}
          in
            fn term => (printFn (strm, term); TextIO.flushOut TextIO.stdOut)
          end

    fun program (strm, PT.PROGRAM m) = let
          val (strm, dcls) = mark (strm, m)
          in
            prNode (strm, "PROGRAM");
            nest strm (fn strm => (prList dcl (strm, dcls)))
          end

    and dcl (strm, obj) = (case obj
           of PT.DCL_Mark m => dcl (mark (strm, m))
            | PT.DCL_Class{name, params, super, vars, meths} => (
                prNode (strm, "DCL_Class");
                nest strm (fn strm => (
                  prId (strm, name);
                  prList param (strm, params);
                  prOpt (fn (strm, (cls, args)) => (
                    prStr (strm, "extends");
                    nest strm (fn strm => (
                      prId (strm, cls);
                      prList exp (strm, args))))
                    ) (strm, super);
                  prList varDcl (strm, vars);
                  prList methDcl (strm, meths))))
            | PT.DCL_Interface{name, extends, meths} => (
                prNode (strm, "DCL_Interface");
                nest strm (fn strm => (
                  prId (strm, name);
                  prOpt
                    (fn (strm, ifc) => prStr (strm, ("extends "^Atom.toString(#tree ifc))))
                      (strm, extends);
                  prList methSpc (strm, meths))))
          (* end case *))

    and param (strm, obj) = (case obj
           of PT.PARAM_Mark m => param (mark (strm, m))
            | PT.PARAM(id, ty) => (
                prNode (strm, "PARAM");
                nest strm (fn strm => (
                  prId (strm, id);
                  typ (strm, ty))))
          (* end case *))

    and typ (strm, obj) = let
          fun str (s, true) = s ^ " OPT"
            | str (s, false) = s
          fun typ' (strm, ty, isOpt) = (case ty
             of PT.TY_Mark ty => let val (strm, ty) = mark(strm, ty) in typ' (strm, ty, isOpt) end
              | PT.TY_Option ty => typ' (strm, ty, true)
              | PT.TY_Id id =>
                  prStr (strm, str(concat["TY_Id(", Atom.toString(#tree id), ")"], isOpt))
              | PT.TY_Bool => prNode (strm, str ("TY_Bool", isOpt))
              | PT.TY_Int => prNode (strm, str ("TY_Int", isOpt))
              | PT.TY_String => prNode (strm, str ("TY_String", isOpt))
              | PT.TY_Void => prNode (strm, str ("TY_Void", isOpt))
            (* end case *))
          in
            typ' (strm, obj, false)
          end

    and varDcl (strm, obj) = (case obj
           of PT.VDCL_Mark m => varDcl (mark (strm, m))
            | PT.VDCL(x, ty, e) => (
                prNode (strm, "VDCL");
                nest strm (fn strm => (
                  prId (strm, x);
                  typ (strm, ty);
                  exp (strm, e))))
          (* end case *))

    and methDcl (strm, obj) = (case obj
           of PT.MDCL_Mark m => methDcl (mark (strm, m))
            | PT.MDCL{override, name, params, retTy, body} => (
                prNode (strm, "MDCL");
                nest strm (fn strm => (
                  if override then prStr (strm, "OVERRIDE") else ();
                  prId (strm, name);
                  prList param (strm, params);
                  typ (strm, retTy);
                  blk (strm, body))))
          (* end case *))

    and methSpc (strm, obj) = (case obj
           of PT.MSPC_Mark m => methSpc (mark (strm, m))
            | PT.MSPC{name, paramTys, retTy} => (
                prNode (strm, "MSPC");
                nest strm (fn strm => (
                  prId (strm, name);
                  prList typ (strm, paramTys);
                  typ (strm, retTy))))
          (* end case *))

    and exp (strm, obj) = (case obj
           of PT.EXP_Mark m => exp (mark (strm, m))
            | PT.EXP_Absorb(e1, e2) => (
                prNode (strm, "EXP_Absorb");
                nest strm (fn strm => (
                  exp (strm, e1);
                  exp (strm, e2))))
            | PT.EXP_Orelse(e1, e2) => (
                prNode (strm, "EXP_Orelse");
                nest strm (fn strm => (
                  exp (strm, e1);
                  exp (strm, e2))))
            | PT.EXP_Andalso(e1, e2) => (
                prNode (strm, "EXP_Andalso");
                nest strm (fn strm => (
                  exp (strm, e1);
                  exp (strm, e2))))
            | PT.EXP_BinOp(e1, rator, e2) => (
                prNode (strm, "EXP_BinOp");
                nest strm (fn strm => (
                  exp (strm, e1);
                  prStr (strm, BinOp.toString rator);
                  exp (strm, e2))))
            | PT.EXP_NegOp e => (
                prNode (strm, "EXP_NegOp");
                nest strm (fn strm => (exp (strm, e))))
            | PT.EXP_New(cls, args) => (
                prNode (strm, "EXP_New");
                nest strm (fn strm => (
                  prId (strm, cls);
                  prList exp (strm, args))))
            | PT.EXP_Apply(memb, args) => (
                prNode (strm, "EXP_Apply");
                nest strm (fn strm => (
                  member (strm, memb);
                  prList exp (strm, args))))
            | PT.EXP_As(cls, arg) => (
                prNode (strm, "EXP_As");
                nest strm (fn strm => (
                  prId(strm, cls);
                  exp (strm, arg))))
            | PT.EXP_Nil tyc => (
                prNode (strm, "EXP_Nil");
                nest strm (fn strm => typ (strm, tyc)))
            | PT.EXP_Select memb => (
                prNode (strm, "EXP_Select");
                nest strm (fn strm => (member (strm, memb))))
            | PT.EXP_Require e => (
                prNode (strm, "EXP_Require");
                nest strm (fn strm => (exp (strm, e))))
            | PT.EXP_Var id => prLeaf (strm, "EXP_Var", Atom.toString(#tree id))
            | PT.EXP_Number n => prLeaf (strm, "EXP_Number", IntInf.toString n)
            | PT.EXP_String s =>
                prLeaf (strm, "EXP_String", String.concat["\"", String.toString s, "\""])
            | PT.EXP_Bool b => prLeaf (strm, "EXP_Bool", Bool.toString b)
          (* end case *))

    and member (strm, obj) = (case obj
           of PT.MEMB_Mark m => member (mark (strm, m))
            | PT.MEMB(e, sel, x) => (
                prNode (strm, "MEMB");
                nest strm (fn strm => (
                  exp (strm, e);
                  prStr (strm, case sel of PT.DOT => "DOT" | PT.REQ => "REQ" | PT.OPT => "OPT");
                  prId (strm, x))))
          (* end case *))

    and blk (strm, obj) = (case obj
           of PT.BLK_Mark m => blk (mark (strm, m))
            | PT.BLK stms => (
                prNode (strm, "BLK");
                nest strm (fn strm => prList stm (strm, stms)))
          (* end case *))

    and stm (strm, obj) = (case obj
           of PT.STM_Mark m => stm (mark (strm, m))
            | PT.STM_Var(id, e) => (
                prNode (strm, "STM_Var");
                nest strm (fn strm => (
                  prId (strm, id);
                  exp (strm, e))))
            | PT.STM_While(e, b) => (
                prNode (strm, "STM_While");
                nest strm (fn strm => (
                  exp (strm, e);
                  blk (strm, b))))
            | PT.STM_If(conds, optElse) => (
                prNode (strm, "STM_If");
                nest strm (fn strm => (
                  prList
                    (fn (strm, {tree=(e, b), ...}) => (exp (strm, e); blk (strm, b)))
                      (strm, conds);
                  prOpt blk (strm, optElse))))
            | PT.STM_Return optE => (
                prNode (strm, "STM_Return");
                nest strm (fn strm => (
                  prOpt exp (strm, optE))))
            | PT.STM_Call e => (
                prNode (strm, "STM_Call");
                nest strm (fn strm => (exp (strm, e))))
            | PT.STM_FieldAssign(e1, e2) => (
                prNode (strm, "STM_FieldAssign");
                nest strm (fn strm => (
                  exp (strm, e1);
                  exp (strm, e2))))
            | PT.STM_VarAssign(x, e) => (
                prNode (strm, "STM_VarAssign");
                nest strm (fn strm => (
                  prId (strm, x);
                  exp (strm, e))))
          (* end case *))

  end
