(* member-var.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure MemberVar : sig

    type t = Types.memb_var

  (* make a member variable *)
    val new : Atom.atom * Types.t -> t

  (* return the name of the member variable *)
    val nameOf : t -> string

  (* return the type of the member variable *)
    val typeOf : t -> Types.t

  (* are two member variables the same? *)
    val same : t * t -> bool

  (* compare two member variables for order; this relation is used to determine
   * the canonical order of member functions in an interface or class signature.
   *)
    val compare : t * t -> order

  (* finite maps and hash tables keyed by member variables *)
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = datatype Types.memb_var

    fun new (name, typ) = MVar{name = name, typ = typ}

    fun nameOf (MVar{name, ...}) = Atom.toString name

    fun typeOf (MVar{typ, ...}) = typ

    fun same (MVar{name=x, ...}, MVar{name=y, ...}) = Atom.same (x, y)

    fun compare (MVar{name=x, ...}, MVar{name=y, ...}) = Atom.lexCompare (x, y)

  (* since member-variable names are unique to a class, we use the names as keys *)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  (* since member-variable names are unique to a class, we use the names as keys *)
    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (MVar{name, ...}) = Atom.hash name
        val sameKey = same
      end)

  end
