(* prim-op.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Primitive operators for SOIR.
 *)

structure PrimOp =
  struct

    datatype t
      = isNil           (* test if a value is nil *)
      | isNotNil        (* test if a value is non-nil *)
      | BoolNot         (* boolean negation *)
      | BoolEqu         (* boolean equality test *)
      | BoolNEq         (* boolean inequality test *)
      | IntAdd          (* integer addition *)
      | IntSub          (* integer subtraction *)
      | IntMul          (* integer multiplication *)
      | IntDiv          (* integer division *)
      | IntEqu          (* integer equality test *)
      | IntNEq          (* integer inequality test *)
      | IntLt           (* integer less-than test *)
      | IntLte          (* integer less-than-or-equal test *)
      | IntGt           (* integer greater-than test *)
      | IntGte          (* integer greater-than-or-equal test *)
      | StrEqu          (* string equality test *)
      | StrNEq          (* string inequality test *)
      | ObjEqu          (* object equality test *)
      | ObjNEq          (* object inequality test *)

    fun toString rator = (case rator
           of isNil => "isNil"
            | isNotNil => "isNotNil"
            | BoolNot => "BoolNot"
            | BoolEqu => "BoolEqu"
            | BoolNEq => "BoolNEq"
            | IntAdd => "IntAdd"
            | IntSub => "IntSub"
            | IntMul => "IntMul"
            | IntDiv => "IntDiv"
            | IntEqu => "IntEqu"
            | IntNEq => "IntNEq"
            | IntLt => "IntLt"
            | IntLte => "IntLte"
            | IntGt => "IntGt"
            | IntGte => "IntGte"
            | StrEqu => "StrEqu"
            | StrNEq => "StrNEq"
            | ObjEqu => "ObjEqu"
            | ObjNEq => "ObjNEq"
          (* end case *))

    fun same (op1 : t, op2) = (op1 = op2)

    fun arity isNil = 1
      | arity isNotNil = 1
      | arity BoolNot = 1
      | arity _ = 2

  end
