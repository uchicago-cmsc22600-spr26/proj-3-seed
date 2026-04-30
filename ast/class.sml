(* class.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Class : sig

    type t = Types.class_tyc

  (* create a new (uninitialized) class type constructor with the given name
   * and (optional) superclass
   *)
    val new : Atom.atom * t option -> t

  (* initialize an interface with its parameters, member variables, and
   * member functions.  The member variables and member functions will
   * will be sorted into a canonical order to make subtype tests more
   * efficient.
   *)
    val init : t * Types.t list * MemberVar.t list * MemberFun.t list -> unit

  (* return the name of a class *)
    val nameOf : t -> string

  (* is the class an internal primitive class? *)
    val isPrim : t -> bool

  (* return the parameter types of a class *)
    val paramTypesOf : t -> Types.t list

  (* return the super-class of a class (if any) *)
    val super : t -> t option

  (* return the member-variables of a class *)
    val varsOf : t -> MemberVar.t list

  (* return the member-function of a class *)
    val funsOf : t -> MemberFun.t list

  (* return the member variable with the given name, or NONE if it does not exist *)
    val findVar : t * Atom.atom -> MemberVar.t option

  (* return the member function with the given name, or NONE if it does not exist *)
    val findFun : t * Atom.atom -> MemberFun.t option

  (* are two classes the same? *)
    val same : t * t -> bool

  (* compare two classes for order *)
    val compare : t * t -> order

  (* finite maps and hash tables keyed by classes *)
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = datatype Types.class_tyc

    fun new (name, super) = ClsTyc(ref{
            name = name, prim = false, params = [], super = super, vars = [], funs = []
          })

    fun init (ClsTyc(r as ref {name, prim, super, ...}), params, vars, fns) =
          r := {
              name = name, prim = prim, params = params, super = super,
              vars = Util.sortMemberVars vars,
              funs = Util.sortMemberFuns fns
            }

    fun nameOf (ClsTyc(ref{name, ...})) = Atom.toString name

    fun isPrim (ClsTyc(ref{prim, ...})) = prim

    fun paramTypesOf (clsTyc as ClsTyc(ref{params, ...})) = params

    fun super (ClsTyc(ref{super, ...})) = super

    fun varsOf (ClsTyc(ref{vars, ...})) = vars

    fun funsOf (ClsTyc(ref{funs, ...})) = funs

    fun findVar (ClsTyc(ref{vars, ...}), x) =
          List.find (fn (Types.MVar{name, ...}) => Atom.same(name, x)) vars

    fun findFun (ClsTyc(ref{funs, ...}), f) =
          List.find (fn (Types.MFun{name, ...}) => Atom.same(name, f)) funs

  (* we rely on reference equality for testing class equality *)
    fun same (ClsTyc r1, ClsTyc r2) = (r1 = r2)

  (* compare two classes for order *)
    fun compare (ClsTyc r1, ClsTyc r2) = if (r1 = r2)
          then EQUAL
          else Atom.compare(#name(!r1), #name(!r2))

  (* finite maps for classes *)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)

  (* hash tables for classes; since class names are unique, we use them as keys *)
    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (ClsTyc(ref{name, ...})) = Atom.hash name
        val sameKey = same
      end)

  end

