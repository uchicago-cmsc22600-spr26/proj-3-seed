(* prog-pt.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Program points provide unique labels for SOIR blocks and (optionally) for statements.
 * These labels can then be used as keys into sets, maps, and tables of auxiliary
 * information about the program.
 *)

structure ProgPt :> sig

    type t

  (* create a new, unique, program point value *)
    val new : unit -> t

  (* return a string representation *)
    val toString : t -> string

  (* are two program points the same? *)
    val same : t * t -> bool

  (* sets, maps, and hash tables keyed by program points *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    type t = Stamp.t

    val new = Stamp.new

    fun toString ppt = "L" ^ Stamp.toString ppt

    val same = Stamp.same

    structure Set = Stamp.Set
    structure Map = Stamp.Map
    structure Tbl = Stamp.Tbl

  end
