(* local-var.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Local variables.
 *)

structure LocalVar : sig

  (* representation of local variables in the AST *)
    type t

  (* create a new local variable with the given name and type *)
    val new : Atom.atom * Types.t -> t

  (* return the variable's name *)
    val nameOf : t -> string

  (* return a unique string identifying the variable (for debugging) *)
    val toString : t -> string

  (* return the type of a variable *)
    val typeOf : t -> Types.t

  (* compare two variables for equality *)
    val same : t * t -> bool

  (* compare two variables *)
    val compare : t * t -> order

  (* finite maps and hash tables keyed by local variables *)
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = V of {
        name : string,
        stamp : Stamp.t,
        typ : Types.t
      }

    fun new (id, typ) = V{name = Atom.toString id, stamp = Stamp.new(), typ = typ}

    fun nameOf (V{name, ...}) = name

    fun toString (V{name, stamp, ...}) = concat[name, "$",  Stamp.toString stamp]

    fun typeOf (V{typ, ...}) = typ

    fun same (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.same(s1, s2)

    fun compare (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.compare(s1, s2)

    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (V{stamp, ...}) = Stamp.hash stamp
        val sameKey = same
      end)

  end
