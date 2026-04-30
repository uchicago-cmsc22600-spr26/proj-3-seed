(* interface.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Interface : sig

    type t = Types.interface_tyc

  (* create a new (uninitialized) interface type constructor with the given name *)
    val new : Atom.atom -> t

  (* initialize an interface with its member-function signatures; the member
   * functions will be sorted into a canonical order to make subtype tests
   * more efficient.
   *)
    val init : t * MemberFun.t list -> unit

  (* return the name of an interface *)
    val nameOf : t -> string

  (* return the member-function specifications of an interface *)
    val funsOf : t -> MemberFun.t list

  (* return the member function with the given name, or NONE if it does not exist *)
    val findFun : t * Atom.atom -> MemberFun.t option

  (* are two interfaces the same? *)
    val same : t * t -> bool

  (* compare two interfaces for order *)
    val compare : t * t -> order

  (* finite sets, maps, and hash tables keyed by interfaces *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = datatype Types.interface_tyc

    fun new name = IfcTyc(ref{name = name, funs = []})

    fun init (IfcTyc(r as ref {name, ...}), fns) =
          r := {
              name = name,
              funs = Util.sortMemberFuns fns
            }

    fun nameOf (IfcTyc(ref{name, ...})) = Atom.toString name

    fun funsOf (IfcTyc(ref{funs, ...})) = funs

    fun findFun (IfcTyc(ref{funs, ...}), f) =
          List.find (fn (Types.MFun{name, ...}) => Atom.same(name, f)) funs

  (* we rely on reference equality for testing interface equality *)
    fun same (IfcTyc r1, IfcTyc r2) = (r1 = r2)

  (* compare two interfaces for order *)
    fun compare (IfcTyc r1, IfcTyc r2) = if (r1 = r2)
          then EQUAL
          else Atom.compare(#name(!r1), #name(!r2))

  (* sets of interfaces *)
    structure Set = RedBlackSetFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  (* finite maps for interfaces *)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  (* hash tables for interfaces; since class names are unique, we use them as keys *)
    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (IfcTyc(ref{name, ...})) = Atom.hash name
        val sameKey = same
      end)

  end
