(* util.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Various utility code for the AST to SOIR translation.
 *)

structure Util : sig

    (* convert an AST type to the equivalent SOIR type *)
    val cvtTy : Types.t -> SOIR.ty

    (* make a new local variable from an AST local variable *)
    val newVar : LocalVar.t -> SOIR.var

    (* make a new member variable from the AST member variable and object offset *)
    val newMVar : MemberVar.t * int -> SOIR.mvar

    (* make a SOIR function name for the given AST class and member function *)
    val newFuncForMFun : Class.t -> MemberFun.t -> SOIR.func

  end = struct

    structure IR = SOIR
    structure Ty = Types

    fun cvtTy typ = (case typ
           of Ty.OptTy t => IR.OptionTy(cvtTy t)
            | Ty.ClsTy _ => IR.ObjectTy
            | Ty.IfcTy _ => IR.TupleTy [IR.ObjectTy, IR.IndexTy]
            | Ty.VoidTy => IR.VoidTy
            | Ty.PrimTy(Types.BoolTy) => IR.BoolTy
            | Ty.PrimTy(Types.IntTy) => IR.IntTy
            | Ty.PrimTy(Types.StringTy) => IR.StringTy
            | Ty.ErrorTy => raise Fail "unexpected error type"
          (* end case *))

    fun newVar x = SOIRVar.new(LocalVar.nameOf x, cvtTy(LocalVar.typeOf x))

    fun newMVar (x, offset) = IR.MVAR{
            name = MemberVar.nameOf x,
            ty = cvtTy(MemberVar.typeOf x),
            offset = offset
          }

    fun newFuncForMFun cls mfun = let
          val Ty.FUNTY(paramTys, retTy) = MemberFun.typeOf mfun
          in
            IR.FUNC{
                name = concat[Class.nameOf cls, ".", MemberFun.nameOf mfun],
                stamp = Stamp.new(),
                paramTys = IR.ObjectTy :: List.map cvtTy paramTys,
                retTy = cvtTy retTy
              }
          end

  end
