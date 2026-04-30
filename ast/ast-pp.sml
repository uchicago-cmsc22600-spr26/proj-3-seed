(* ast-pp.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Pretty printer for SooL abstract syntax.
 *)

structure ASTPP : sig

    val verboseMode : bool ref

    val output : TextIO.outstream * string * AST.program -> unit

  end = struct

    structure Ty = Types
    structure MFun = MemberFun
    structure MVar = MemberVar
    structure PP = TextIOPP

    val verboseMode = ref false

    val indent0 = PP.Abs 0
    val indent2 = PP.Abs 2
    val indent4 = PP.Abs 4

    fun ppList ppFn (left, sep, right) (ppStrm, xs) = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          fun pp [] = string right
            | pp [x] = (ppFn x; string right)
            | pp (x::xs) = (ppFn x; string sep; sp(); pp xs)
          in
            string left; pp xs
          end

    fun ppTyp ppStrm typ = PP.string ppStrm (Ty.toString typ)

    fun ppVar ppStrm x =
          PP.string ppStrm (if !verboseMode then LocalVar.toString x else LocalVar.nameOf x)

    fun ppVarBind ppStrm x = (
          PP.openHBox ppStrm;
            ppVar ppStrm x;
            PP.space ppStrm 1;
            PP.string ppStrm ":";
            PP.space ppStrm 1;
            ppTyp ppStrm (LocalVar.typeOf x);
          PP.closeBox ppStrm)

    fun ppExp ppStrm = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          fun pp e = (case e
                 of AST.EXP_Absorb(e1, e2) => ppBinExp (e1, "??", e2)
                  | AST.EXP_Orelse(e1, e2) => ppBinExp (e1, "||", e2)
                  | AST.EXP_Andalso(e1, e2) => ppBinExp (e1, "&&", e2)
                  | AST.EXP_PrimOp(e1, rator, e2) => ppBinExp (e1, BinOp.toString rator, e2)
                  | AST.EXP_New(cls, args) => (
                      PP.openHBox ppStrm;
                        string(Class.nameOf cls);
                        PP.openHOVBox ppStrm indent4;
                          ppList pp ("(", ",", ")") (ppStrm, args);
                        PP.closeBox ppStrm;
                      PP.closeBox ppStrm)
                  | AST.EXP_Apply{obj, opt, f, args, ...} => (
                      PP.openHBox ppStrm;
                        pp obj;
                        string (if opt then "?" else ".");
                        string(MFun.nameOf f);
                        PP.openHOVBox ppStrm indent4;
                          ppList pp ("(", ",", ")") (ppStrm, args);
                        PP.closeBox ppStrm;
                      PP.closeBox ppStrm)
                  | AST.EXP_As(cls, arg) => (
                      PP.openHBox ppStrm;
                        string(Class.nameOf cls);
                        string ".as(";
                        pp arg;
                        string ")";
                      PP.closeBox ppStrm)
                  | AST.EXP_Nil typ => (
                      PP.openHBox ppStrm;
                        string "(nil"; sp(); ppTyp ppStrm typ; string ")";
                      PP.closeBox ppStrm)
                  | AST.EXP_Select{obj, opt, var, ...} => (
                      PP.openHBox ppStrm;
                        pp obj;
                        string (if opt then "?" else ".");
                        string(MemberVar.nameOf var);
                      PP.closeBox ppStrm)
                  | AST.EXP_Require e => (
                      PP.openHBox ppStrm;
                        pp e; sp(); string "!";
                      PP.closeBox ppStrm)
                  | AST.EXP_Var x => ppVar ppStrm x
                  | AST.EXP_Number n => string(IntInf.toString n)
                  | AST.EXP_String s => string(concat["\"", String.toString s, "\""])
                  | AST.EXP_Bool b => string(Bool.toString b)
                  | AST.EXP_Coerce{from, to, e} => (
                      PP.openHBox ppStrm;
                        string "("; ppTyp ppStrm to; sp(); string ":>";
                        sp(); ppTyp ppStrm from; string ")"; pp e;
                      PP.closeBox ppStrm)
                (* end case *))
          and ppBinExp (e1, rator, e2) = (
                PP.openHOVBox ppStrm indent4;
                  string "("; pp e1; sp();
                  PP.openHBox ppStrm;
                    string rator; sp(); pp e2; string ")";
                  PP.closeBox ppStrm;
                PP.closeBox ppStrm)
          in
            pp
          end

    fun ppStm ppStrm = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          val ppExp = ppExp ppStrm
          fun ppRHSExp rhs = (
                sp(); string "=";
                PP.openHOVBox ppStrm indent4;
                  sp(); ppExp rhs; string ";";
                PP.closeBox ppStrm)
        (* pretty print an individual statement with a preceeding newline; we
         * start with a newline so that the indentation of blocks works correctly.
         *)
          fun pp stm = (case stm
                 of AST.STM_Var(x, e) => (
                      nl();
                      PP.openHBox ppStrm;
                        string "var"; sp(); ppVarBind ppStrm x;
                        ppRHSExp e;
                      PP.closeBox ppStrm)
                  | AST.STM_While(cond, AST.BLK stms) => (
                      nl();
                      PP.openVBox ppStrm indent0;
                        PP.openVBox ppStrm indent2;
                          PP.openHBox ppStrm;
                            string "while"; sp(); ppExp cond; sp(); string "{";
                          PP.closeBox ppStrm;
                          List.app pp stms;
                        PP.closeBox ppStrm;
                        nl();
                        string "}";
                      PP.closeBox ppStrm)
                  | AST.STM_If(conds, optElse) => let
                    (* print "[else] if <exp> "{" <stms> "}" *)
                      fun ppIfThen ((e, AST.BLK stms), first) = (
                            nl();
                            PP.openVBox ppStrm indent2;
                              PP.openHBox ppStrm;
                                if first then () else (string "else"; sp());
                                string "if"; sp(); ppExp e; sp(); string "{";
                              PP.closeBox ppStrm;
                              List.app pp stms;
                            PP.closeBox ppStrm;
                            nl();
                            string "}";
                            false)
                      in
                        PP.openVBox ppStrm indent0;
                          ignore (List.foldl ppIfThen true conds);
                          case optElse
                           of SOME(AST.BLK stms) => (
                                nl();
                                PP.openVBox ppStrm indent2;
                                  PP.openHBox ppStrm;
                                    string "else"; sp(); string "{";
                                  PP.closeBox ppStrm;
                                  List.app pp stms;
                                PP.closeBox ppStrm;
                                nl();
                                string "}")
                            | NONE => ()
                          (* end case *);
                        PP.closeBox ppStrm
                      end
                  | AST.STM_Return NONE => (nl(); string "return;")
                  | AST.STM_Return(SOME e) => (
                      nl();
                      PP.openHBox ppStrm;
                        string "return"; sp(); ppExp e; string ";";
                      PP.closeBox ppStrm)
                  | AST.STM_Call{obj, opt, f, args, ...} => (
                      nl();
                      PP.openHBox ppStrm;
                        ppExp obj;
                        string (if opt then "?" else ".");  (* should always be false!!! *)
                        string(MFun.nameOf f);
                        PP.openHOVBox ppStrm indent4;
                          ppList ppExp ("(", ",", ");") (ppStrm, args);
                        PP.closeBox ppStrm;
                      PP.closeBox ppStrm)
                  | AST.STM_FieldAssign{obj, var, rhs, ...} => (
                      nl();
                      PP.openHBox ppStrm;
                        ppExp obj;
                        string ".";
                        string(MemberVar.nameOf var);
                        ppRHSExp rhs;
                      PP.closeBox ppStrm)
                  | AST.STM_VarAssign(x, e) => (
                      nl();
                      PP.openHBox ppStrm;
                        ppVar ppStrm x;
                        ppRHSExp e;
                      PP.closeBox ppStrm)
                (* end case *))
          in
            pp
          end

    fun ppDcl ppStrm dcl = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          val ppTyp = ppTyp ppStrm
          fun ppParams params = ppList (ppVarBind ppStrm) ("(", ",", ")") (ppStrm, params)
          in
            case dcl
             of AST.DCL_Class{tyc, params, super, vars, funs} => let
                  fun ppMVar (AST.VDCL(x, e)) = (
                        nl();
                        PP.openHBox ppStrm;
                          string "var"; sp(); string(MVar.nameOf x); sp();
                          string ":"; sp(); ppTyp(MVar.typeOf x); sp(); string "=";
                          PP.openHOVBox ppStrm indent4;
                            sp(); ppExp ppStrm e;
                          PP.closeBox ppStrm;
                        PP.closeBox ppStrm)
                  fun ppMFun (AST.FDCL{override, name, self, params, retTy, body=AST.BLK stms}) = (
                        nl();
                        PP.openVBox ppStrm indent0;
                          PP.openVBox ppStrm indent2;
                            PP.openHBox ppStrm;
                              if override then (string "override"; sp()) else ();
                              string "method"; sp(); string(MFun.nameOf name); sp();
                              ppParams (self::params); sp(); string "->"; sp();
                              ppTyp retTy;
                              sp(); string "{";
                            PP.closeBox ppStrm;
                            List.app (ppStm ppStrm) stms;
                          PP.closeBox ppStrm;
                          nl();
                          string "}";
                        PP.closeBox ppStrm)
                  in
                    PP.openVBox ppStrm indent2;
                      PP.openHBox ppStrm;
                        string "class"; sp(); string(Class.nameOf tyc); sp();
                        ppParams params;
                        case super
                         of SOME(tyc', args) => (
                              sp(); string "extends"; sp(); string(Class.nameOf tyc');
                              ppList (ppExp ppStrm) ("(", ",", ")") (ppStrm, args))
                          | NONE => ()
                        (* end case *);
                        sp(); string "{";
                      PP.closeBox ppStrm;
                      List.app ppMVar vars;
                      List.app ppMFun funs;
                    PP.closeBox ppStrm;
                    nl();
                    string "}"
                  end
              | AST.DCL_Interface ifc => let
                  fun ppFunSpc f = (
                        nl();
                        PP.openHBox ppStrm;
                          string "method"; sp(); string(MFun.nameOf f); sp();
                          ppList ppTyp ("(", ",", ")") (ppStrm, MFun.paramTysOf f);
                          sp(); string "->"; sp();
                          ppTyp (MFun.returnTyOf f);
                        PP.closeBox ppStrm)
                  in
                    PP.openVBox ppStrm indent2;
                      PP.openHBox ppStrm;
                        string "interface"; sp(); string(Interface.nameOf ifc); sp(); string "{";
                      PP.closeBox ppStrm;
                      List.app ppFunSpc (Interface.funsOf ifc);
                    PP.closeBox ppStrm;
                    nl();
                    string "}"
                  end
            (* end case *)
          end

    fun output (outS, message, AST.PROGRAM{dcls, ...}) = let
          val ppStrm = PP.openOut {dst = outS, wid = 100}
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          in
            PP.openVBox ppStrm (PP.Abs 0);
              string (concat ["/* AST: ", message, " */"]); nl();
              List.app (fn dcl => (ppDcl ppStrm dcl; nl())) dcls;
              string "/* Program end */"; nl();
            PP.closeBox ppStrm;
            PP.closeStream ppStrm
          end

  end
