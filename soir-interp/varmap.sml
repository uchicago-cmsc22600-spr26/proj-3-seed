(* varmap.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure VarMap : sig

    type t

  (* subset of the ORD_MAP operations with monomorphic types *)
    val singleton : SOIRVar.t * Value.t -> t
    val insert : t * SOIRVar.t * Value.t -> t
    val find : t * SOIRVar.t -> Value.t ref option

    val dump : TextIO.outstream * t -> unit

  end = struct

    structure VMap = SOIRVar.Map

    type t = Value.t ref VMap.map

    fun singleton (x, v) = VMap.singleton(x, ref v)

    fun insert (vmap : t, x, v) = VMap.insert(vmap, x, ref v)

    val find : t * SOIRVar.t -> Value.t ref option = VMap.find

    fun dump (outS, vmap) = let
          fun pr (x, v) = TextIO.output(outS, concat[
                  "    ", StringCvt.padRight #" " 24 (SOIRVar.nameOf x),
                  " --> ", Value.toString(!v), "\n"
                ])
          in
            TextIO.output(outS, "=== ENVIRONMENT ===\n");
            VMap.appi pr vmap;
            TextIO.output(outS, "===\n")
          end

  end

