(* soir-index.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure SOIRIndex : sig

    type t = SOIR.index_table

  (* make a SOIR index table for the given AST class and AST interface.  The contents is
   * initially empty.
   *)
    val new : Class.t * Interface.t -> t

  (* fill in the items of an empty table *)
    val fillIndex : t * SOIR.item list -> unit

  (* return the interface implemented by a table *)
    val interfaceOf : t -> Interface.t

  (* are two tables the same? *)
    val same : t * t -> bool

  end = struct

    structure IR = SOIR

    datatype t = datatype IR.index_table

    fun new (cls, ifc) = INDEX{
            cls = cls, ifc = ifc,
            name = concat[Class.nameOf cls, "@", Interface.nameOf ifc],
            stamp = Stamp.new(),
            items = ref[]
          }

    fun fillIndex (INDEX{items as ref[], ...}, items') = (items := items')
      | fillIndex (INDEX{name, ...}, _) = raise Fail(concat["table ", name, " is already full"])

    fun interfaceOf (INDEX{ifc, ...}) = ifc

    fun same (INDEX{stamp=a, ...}, INDEX{stamp=b, ...}) = Stamp.same (a, b)

  end
