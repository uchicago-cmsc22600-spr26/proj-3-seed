(* soir-var.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure SOIRVar : sig

    type t = SOIR.var

  (* create a new local variable with the given type *)
    val new : string * SOIR.ty -> t

  (* return the name of a variable *)
    val nameOf : t -> string

  (* return the type of a variable *)
    val typeOf : t -> SOIRType.t

  (* are two variables the same? *)
    val same : t * t -> bool

  (* compare variables for order *)
    val compare : t * t -> order

    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t

  end = struct

    datatype t = datatype SOIR.var

    fun new (name, ty) = VAR{name = name, stamp = Stamp.new(), ty = ty}

    fun nameOf (VAR{name, stamp, ...}) = concat[name, "_", Stamp.toString stamp]

    fun typeOf (VAR{ty, ...}) = ty

    fun same (VAR{stamp=s1, ...}, VAR{stamp=s2, ...}) = Stamp.same(s1, s2)

    fun compare (VAR{stamp=s1, ...}, VAR{stamp=s2, ...}) = Stamp.compare(s1, s2)

    structure Set = RedBlackSetFn (
      struct
        type ord_key = t
        val compare = compare
      end)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  end
