(* type-of.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure TypeOf : sig

  (* return the type of an AST expression *)
    val exp : AST.exp -> Types.t

  end = struct

    structure Ty = Types

  (* return the result type of an operator *)
    fun operator rator = (case rator
           of BinOp.^<^ => Ty.boolTy
            | BinOp.^<=^ => Ty.boolTy
            | BinOp.^==^ => Ty.boolTy
            | BinOp.^!=^ => Ty.boolTy
            | BinOp.^@^ => Ty.stringTy
            | BinOp.^+^ => Ty.intTy
            | BinOp.^-^ => Ty.intTy
            | BinOp.^*^ => Ty.intTy
            | BinOp.^/^ => Ty.intTy
          (* end case *))

  (* add an option to a type (assuming that it doesn't already have an option) *)
    fun optionTy (ty as Ty.OptTy _) = ty
      | optionTy ty = Ty.OptTy ty

    fun exp e = (case e
           of AST.EXP_Absorb(_, e2) => exp e2
            | AST.EXP_Orelse _ => Ty.boolTy
            | AST.EXP_Andalso _ => Ty.boolTy
            | AST.EXP_PrimOp(_, rator, _) => operator rator
            | AST.EXP_New(cls, _) => Ty.ClsTy cls
            | AST.EXP_Apply{f, opt=true, ...} => optionTy(MemberFun.returnTyOf f)
            | AST.EXP_Apply{f, opt=false, ...} => MemberFun.returnTyOf f
            | AST.EXP_As(cls, _) => optionTy(Ty.ClsTy cls)
            | AST.EXP_Select{var, opt=true, ...} => optionTy(MemberVar.typeOf var)
            | AST.EXP_Select{var, opt=false, ...} => MemberVar.typeOf var
            | AST.EXP_Require e => (case exp e
                 of Ty.OptTy ty => ty
                  | _ => raise Fail "bogus argument type for EXP_Require"
                (* end case *))
            | AST.EXP_Var x => LocalVar.typeOf x
            | AST.EXP_Number _ => Ty.intTy
            | AST.EXP_String _ => Ty.stringTy
            | AST.EXP_Bool _ => Ty.boolTy
            | AST.EXP_Nil typ => Ty.OptTy typ
            | AST.EXP_Coerce{to, ...} => to
          (* end case *))

  end
