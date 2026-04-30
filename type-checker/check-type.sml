(* check-type.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Type checker for SooL type expressions.
 *)

structure CheckType : sig

  (* check the well-formedness of a type and convert it to a Types.t value *)
    val check : Env.t * ParseTree.typ -> Types.t

  (* check the well-formedness of a return type *)
    val checkReturnType : Env.t * ParseTree.typ -> Types.t

  end = struct

    structure PT = ParseTree
    structure Ty = Types
    structure Err = TypeError

  (* import the token datatype to get access to the constructors without qualification *)
    datatype token = datatype Err.token

    fun check (env, ty) = (case ty
           of PT.TY_Mark{span, tree} => check (Env.setSpan(env, span), tree)
            | PT.TY_Option typ => Ty.OptTy(check (env, typ))
            | PT.TY_Id{span, tree} => (case Env.findTyc(env, tree)
                 of SOME typ => typ
                  | NONE => (
                      Err.error (Env.setSpan(env, span), [
                          S "unknown class/interface ", A tree, S " in type expression"
                        ]);
                    (* return ErrorTy to avoid cascading errors *)
                      Ty.ErrorTy)
                (* end case *))
            | PT.TY_Bool => Ty.PrimTy Ty.BoolTy
            | PT.TY_Int => Ty.PrimTy Ty.IntTy
            | PT.TY_String => Ty.PrimTy Ty.StringTy
            | PT.TY_Void => raise Fail "unexpected TY_Void"
          (* end case *))

    fun checkReturnType (env, PT.TY_Mark{span, tree}) =
          checkReturnType (Env.setSpan(env, span), tree)
      | checkReturnType (env, PT.TY_Void) = Ty.VoidTy
      | checkReturnType (env, ty) = check (env, ty)

  end
