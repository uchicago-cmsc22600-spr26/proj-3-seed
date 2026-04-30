(* member-fun.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure MemberFun : sig

    type t = Types.memb_fun

  (* make a member function *)
    val new : Atom.atom * Types.t list * Types.t -> t

  (* return the name of the member function *)
    val nameOf : t -> string

  (* return the type signature of a member function *)
    val typeOf : t -> Types.fun_sig

  (* return the parameter types of a member function *)
    val paramTysOf : t -> Types.t list

  (* return the return type of a member function *)
    val returnTyOf : t -> Types.t

  (* are two member functions the same? *)
    val same : t * t -> bool

  (* compare two member functions for order; this relation is used to determine
   * the canonical order of member functions in an interface or class signature.
   *)
    val compare : t * t -> order

  (* finite maps and hash tables keyed by member functions *)
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = datatype Types.memb_fun

    fun new (name, paramTys, retTy) = MFun{name = name, sign = Types.FUNTY(paramTys, retTy)}

    fun nameOf (MFun{name, ...}) = Atom.toString name

    fun typeOf (MFun{sign, ...}) = sign

    fun paramTysOf (MFun{sign=Types.FUNTY(tys, _), ...}) = tys

    fun returnTyOf (MFun{sign=Types.FUNTY(_, ty), ...}) = ty

    fun same (MFun{name=f, ...}, MFun{name=g, ...}) = Atom.same (f, g)

    fun compare (MFun{name=f, ...}, MFun{name=g, ...}) = Atom.lexCompare (f, g)

  (* since member-function names are unique to a class, we use the names as keys *)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  (* since member-function names are unique to a class, we use the names as keys *)
    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (MFun{name, ...}) = Atom.hash name
        val sameKey = same
      end)

  end
