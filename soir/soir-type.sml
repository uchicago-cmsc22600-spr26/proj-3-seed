(* soir-type.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure SOIRType : sig

    type t = SOIR.ty

    val toString : t -> string

  end = struct

    datatype t = datatype SOIR.ty

    fun toString ty = (case ty
           of OptionTy ty => toString ty ^ "?"
            | ObjectTy => "object"
            | ClassTy => "class_metadata"
            | IndexTy => "index_table"
            | FuncTy(tys, retTy) =>
                concat["(", String.concatWithMap "," toString tys, ")->", toString retTy]
            | TupleTy tys => concat["<", String.concatWithMap "," toString tys, ">"]
            | BoolTy => "bool"
            | IntTy => "int"
            | StringTy => "string"
            | VoidTy => "void"
            | NilTy => "nil"
          (* end case *))

  end
