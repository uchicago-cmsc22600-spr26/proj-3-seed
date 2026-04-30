(* binop.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * SooL binary operators.
 *)

structure BinOp : sig

    datatype t = ^<^ | ^<=^ | ^==^ | ^!=^ | ^@^ | ^+^ | ^-^ | ^*^ | ^/^

  (* is a binary operator either '==' or '!=' *)
    val isEqOp : t -> bool

  (* string representation of operator *)
    val toString : t -> string

  (* equality test *)
    val same : t * t -> bool

  end = struct

    datatype t = ^<^ | ^<=^ | ^==^ | ^!=^ | ^@^ | ^+^ | ^-^ | ^*^ | ^/^

    fun isEqOp ^==^ = true
      | isEqOp ^!=^ = true
      | isEqOp _ = false

    fun toString ^<^  = "<"
      | toString ^<=^ = "'<='"
      | toString ^==^ = "'=='"
      | toString ^!=^ = "'!='"
      | toString ^@^  = "'@'"
      | toString ^+^  = "'+'"
      | toString ^-^  = "'-'"
      | toString ^*^  = "'*'"
      | toString ^/^  = "'/'"

    val same : t * t -> bool = (op =)

  end
