(* internal-classes.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure InternalClasses : sig

  (* skeleton class declarations for the internal classes; these are used to compute
   * class layout for the classes.
   * NOTE: these are not for translation to SOIR, since the are incomplete!!!
   *)
    val boolCls : AST.dcl
    val intCls : AST.dcl
    val stringCls : AST.dcl
    val systemCls : AST.dcl

  end = struct

    structure Ty = Types

    val self = Atom.atom "self"

  (* create a skeleton class declaration *)
    fun mkDcl (tyc as Ty.ClsTyc(ref{name, params, vars, funs, ...})) = let
          fun mkParam (i, ty) = LocalVar.new(Atom.atom("p"^Int.toString i), ty)
          fun mkFun f = AST.FDCL{
                  override = false, name = f,
                  self = LocalVar.new(self, Ty.ClsTy tyc),
                  params = List.mapi mkParam (MemberFun.paramTysOf f),
                  retTy = MemberFun.returnTyOf f,
                  body = AST.BLK[]
                }
          val params = List.mapi mkParam params
          val vars = (case (params, vars)
                 of ([p], [x]) => [AST.VDCL(x, AST.EXP_Var p)]
                  | ([], []) => []
                  | _ => raise Fail "impossible"
                (* end case *))
          in
            AST.DCL_Class{
                tyc = tyc, params = params, super = NONE,
                vars = vars, funs = List.map mkFun funs
              }
          end

    val boolCls   = mkDcl Basis.boolCls
    val intCls    = mkDcl Basis.intCls
    val stringCls = mkDcl Basis.stringCls
    val systemCls = mkDcl Basis.systemCls

  end

