(* soir-func.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure SOIRFunc : sig

    type t = SOIR.func

  (* create a new member function f for the given class with the given type *)
    val new : {cls : string, f : string, ty : SOIR.ty list * SOIR.ty} -> t

  (* create a function with the given name and type *)
    val new' : string *  SOIR.ty list * SOIR.ty -> t

  (* return the name of a function *)
    val nameOf : t -> string

  (* return the function type of the function *)
    val typeOf : t -> SOIRType.t

  (* return the function's parameter types *)
    val typeOfParams : t -> SOIRType.t list

  (* return the function's return type *)
    val returnTypeOf : t -> SOIRType.t

  (* are two functions the same? *)
    val same : t * t -> bool

  (* compare functions for order *)
    val compare : t * t -> order

  (* finite maps and hash tables keyed by functions *)
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = datatype SOIR.func

    fun new' (name, paramTys, retTy) = FUNC{
            name = name, stamp = Stamp.new(), paramTys = paramTys, retTy = retTy
          }

    fun new {cls, f, ty=(paramTys, retTy)} = new' (concat[cls, ".", f], paramTys, retTy)

    fun nameOf (FUNC{name, ...}) = name

    fun typeOf (FUNC{paramTys, retTy, ...}) = SOIR.FuncTy(paramTys, retTy)

    fun typeOfParams (FUNC{paramTys, ...}) = paramTys

    fun returnTypeOf (FUNC{retTy, ...}) = retTy

    fun same (FUNC{stamp=s1, ...}, FUNC{stamp=s2, ...}) = Stamp.same(s1, s2)

    fun compare (FUNC{stamp=s1, ...}, FUNC{stamp=s2, ...}) = Stamp.compare(s1, s2)

    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (FUNC{stamp, ...}) = Stamp.hash stamp
        val sameKey = same
      end)

  end
