(* type-util.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure TypeUtil : sig

    datatype relation
      = EQUTY           (* types are equal; or one is an ErrorTy *)
      | SUBTY           (* first type is a subtype of the second *)
      | MISMATCH        (* first type is *not* a subtype of the second *)

  (* compare two types for equality or subtyping *)
    val compare : Types.t * Types.t -> relation

  (* compare two types for equality *)
    val equal : Types.t * Types.t -> bool

  (* `subtype(ty1, ty2)` returns true if `ty1` is a subtype of (or equal to) `ty2` *)
    val subtype : Types.t * Types.t -> bool

  (* `join(ty1, ty2)` returns `SOME ty1` if `subtype(ty1, ty2)` is true,
   * returns `SOME ty2` if `subtype(ty2, ty1)` is true, and returns `NONE`
   * if the two types are not in a subtype relationship.
   *)
    val join : Types.t * Types.t -> Types.t option

  (* does a class implement an interface? *)
    val implements : Class.t * Interface.t -> bool

  (* `primToObjectTy ty` returns `NONE` if ty is not a primitive (or optional primitive) type.
   * (i.e., `ty` is an object or optional object type already).  Otherwise, it returns
   * `SOME ty'`, where `ty'` is the interface (or optional interface) type for the primitive
   * type ty.
   *)
    val primToObjectTy : Types.t -> Types.t option

  (* add an option to a type (assuming that it doesn't already have an option) *)
    val optionTy : Types.t -> Types.t

  end = struct

    structure Ty = Types

    datatype relation
      = EQUTY           (* types are equal *)
      | SUBTY           (* first type is a subtype of the second *)
      | MISMATCH        (* first type is *not* a subtype of the second *)

    fun equal (Ty.OptTy ty1, Ty.OptTy ty2) = equal (ty1, ty2)
      | equal (Ty.ClsTy(Ty.ClsTyc r1), Ty.ClsTy(Ty.ClsTyc r2)) = (r1 = r2)
      | equal (Ty.IfcTy(Ty.IfcTyc r1), Ty.IfcTy(Ty.IfcTyc r2)) = (r1 = r2)
      | equal (Ty.PrimTy Ty.BoolTy, Ty.PrimTy Ty.BoolTy) = true
      | equal (Ty.PrimTy Ty.IntTy, Ty.PrimTy Ty.IntTy) = true
      | equal (Ty.PrimTy Ty.StringTy, Ty.PrimTy Ty.StringTy) = true
      | equal (Ty.VoidTy, Ty.VoidTy) = true
      | equal (Ty.ErrorTy, _) = true  (* error type is equal to anything *)
      | equal (_, Ty.ErrorTy) = true
      | equal _ = false

  (* compare two canonically ordered lists of member functions (members are
   * sorted by name in increasing order).  If the two lists are equal, then
   * return EQUTY, if the first list is a superset of the second, then
   * return SUBTY, otherwise return MISMATCH.
   *)
    fun compareFuns ([], []) = EQUTY
      | compareFuns (_, []) = SUBTY
      | compareFuns ([], _) = MISMATCH
      | compareFuns (f::fs, g::gs) = (case MemberFun.compare(f, g)
           of LESS => (case compareFuns (fs, g::gs)
                 of MISMATCH => MISMATCH
                  | _ => SUBTY
                (* end case *))
            | EQUAL => let
                val Ty.FUNTY(paramTys1, retTy1) = MemberFun.typeOf f
                val Ty.FUNTY(paramTys2, retTy2) = MemberFun.typeOf g
                fun chkParams () = if ListPair.allEq equal (paramTys1, paramTys2)
                      then compareFuns (fs, gs)
                      else MISMATCH
                in
                  if equal(retTy1, retTy2) then chkParams() else MISMATCH
                end
            | GREATER => MISMATCH
          (* end case *))

    fun compare (ty1, ty2) = (case (ty1, ty2)
         of (Ty.OptTy ty1, Ty.OptTy ty2) => compare (ty1, ty2)
          | (_, Ty.OptTy ty2) => compareSub (ty1, ty2)
          | (Ty.OptTy _, _) => MISMATCH
          | (Ty.ClsTy(cls1 as Ty.ClsTyc r1), Ty.ClsTy(cls2 as Ty.ClsTyc r2)) =>
              if (r1 = r2)
                then EQUTY
              else if inherits(cls1, cls2)
                then SUBTY
                else MISMATCH
          | (Ty.ClsTy(cls as Ty.ClsTyc _), Ty.IfcTy(ifc as Ty.IfcTyc _)) =>
              if implements(cls, ifc)
                then SUBTY
                else MISMATCH
          | (Ty.IfcTy(ifc1 as Ty.IfcTyc r1), Ty.IfcTy(ifc2 as Ty.IfcTyc r2)) =>
              if (r1 = r2)
                then EQUTY
                else extends(ifc1, ifc2)
          | (Ty.PrimTy Ty.BoolTy, Ty.PrimTy Ty.BoolTy) => EQUTY
          | (Ty.PrimTy Ty.IntTy, Ty.PrimTy Ty.IntTy) => EQUTY
          | (Ty.PrimTy Ty.StringTy, Ty.PrimTy Ty.StringTy) => EQUTY
        (* primitive types are also subtypes of their interfaces *)
          | (Ty.PrimTy pty, ty) => compareSub (Ty.IfcTy(BasisEnv.interfaceOf pty), ty)
          | (Ty.VoidTy, _) => raise Fail "unexpected 'void' type"
          | (_, Ty.VoidTy) => raise Fail "unexpected 'void' type"
          | (Ty.ErrorTy, _) => EQUTY (* error type is equal to anything *)
          | (_, Ty.ErrorTy) => EQUTY
          | _ => MISMATCH
        (* end case *))

  (* compare two types and return SUBTY if they are equal or ty1 <: ty2 *)
    and compareSub (ty1, ty2) = (case compare(ty1, ty2)
               of MISMATCH => MISMATCH
                | _ => SUBTY
              (* end case *))

    and inherits (Ty.ClsTyc(ref{super=NONE, ...}), _) = false
      | inherits (Ty.ClsTyc(ref{super=SOME super, ...}), cls : Class.t) =
          Class.same(super, cls) orelse inherits(super, cls)

    and implements (Ty.ClsTyc(ref{funs=fns1, ...}), Ty.IfcTyc(ref{funs=fns2, ...})) = (
          case compareFuns(fns1, fns2)
           of MISMATCH => false
            | _ => true
          (* end case *))

    and extends (Ty.IfcTyc(ref{funs=fns1, ...}), Ty.IfcTyc(ref{funs=fns2, ...})) =
          compareFuns(fns1, fns2)

    fun subtype (ty1, ty2) = (case compare(ty1, ty2)
           of MISMATCH => false
            | _ => true
          (* end case *))

    fun join (ty1, ty2) = (
          case compare(ty1, ty2)
           of MISMATCH => (case compare(ty2, ty1)
                 of MISMATCH => NONE
                  | _ => SOME ty1
                (* end case *))
            | _ => SOME ty2
          (* end case *))

    fun primToObjectTy (Ty.OptTy ty) = Option.map Ty.OptTy (primToObjectTy ty)
      | primToObjectTy (Ty.PrimTy pty) = SOME(Types.IfcTy(BasisEnv.interfaceOf pty))
      | primToObjectTy _ = NONE

  (* add an option to a type (assuming that it doesn't already have an option) *)
    fun optionTy (ty as Ty.OptTy _) = ty
      | optionTy Ty.VoidTy = raise Fail "unexpected 'void' type"
      | optionTy Ty.ErrorTy = Ty.ErrorTy
      | optionTy ty = Ty.OptTy ty

  end
