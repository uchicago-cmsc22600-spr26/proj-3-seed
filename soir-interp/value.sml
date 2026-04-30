(* value.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Runtime values for the SOIR interpreter.
 *)

structure Value =
  struct

    local
      structure IR = SOIR
    in

    datatype t
      = UNDEF                           (* undefined (from uninitialized storage) *)
      | VOID                            (* used for void return *)
      | BOOL of bool
      | INT of IntInf.int
      | STR of string
      | NIL
      | FUNC of IR.func
      | META of IR.class
      | INDEX of IR.index_table
      | OBJ of t array               (* location 0 will be class metadata *)
      | TUPLE of t list

    fun toString v = let
          fun toS (OBJ _, 0) = "<object>"
            | toS (TUPLE _, 0) = "<tuple>"
            | toS (UNDEF, _) = "<undefined>"
            | toS (VOID, _) = "<void>"
            | toS (BOOL b, _) = Bool.toString b
            | toS (INT n, _) = if (n < 0)
                then "-" ^ IntInf.toString(~n)
                else IntInf.toString n
            | toS (STR s, _) = String.concat["\"", String.toString s, "\""]
            | toS (NIL, _) = "<nil>"
            | toS (FUNC f, _) = SOIRFunc.nameOf f
            | toS (META(IR.CLASS{tyc, ...}), _) = Class.nameOf tyc
            | toS (INDEX(IR.INDEX{name, ...}), _) = name
            | toS (OBJ arr, d) = String.concat[
                  "{", String.concatWithMap "," (fn v => toS(v, d-1)) (Array.toList arr), "}"
                ]
            | toS (TUPLE tpl, d) = String.concat[
                  "<", String.concatWithMap "," (fn v => toS(v, d-1)) tpl, ">"
                ]
          in
            toS (v, 5)
          end

    end (* local *)

  end

