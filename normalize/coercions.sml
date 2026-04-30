(* coercions.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Coercions : sig

  (* object-object coercions *)
    datatype t
      = ClsCls of Class.t * Class.t             (* (B ≽ C) *)
      | IfcCls of Interface.t * Class.t         (* (I ≽ C) *)
      | IfcIfc of Interface.t * Interface.t     (* (I ≽ J) *)

  (* convert a coercion to a string (for debugging) *)
    val toString : t -> string

  (* sets of object-to-object coercions *)
    structure Set : ORD_SET where type Key.ord_key = t

  end = struct

    datatype t
      = ClsCls of Class.t * Class.t             (* (B ≽ C) *)
      | IfcCls of Interface.t * Class.t         (* (I ≽ C) *)
      | IfcIfc of Interface.t * Interface.t     (* (I ≽ J) *)

    fun toString c = let
          fun toS (a, b) = concat["(", a, " ≽ ", b, ")"]
          in
            case c
             of ClsCls(cls1, cls2) => toS(Class.nameOf cls1, Class.nameOf cls2)
              | IfcCls(ifc, cls) => toS(Interface.nameOf ifc, Class.nameOf cls)
              | IfcIfc(ifc1, ifc2) => toS(Interface.nameOf ifc1, Interface.nameOf ifc2)
            (* end case *)
          end

    structure Set = RedBlackSetFn (
      struct
        type ord_key = t
        fun compare (ClsCls(cls11, cls12), ClsCls(cls21, cls22)) = (
              case Class.compare(cls11, cls21)
               of EQUAL => Class.compare(cls12, cls22)
                | order => order
              (* end case *))
          | compare (ClsCls _, _) = LESS
          | compare (_, ClsCls _) = GREATER
          | compare (IfcCls(ifc1, cls1), IfcCls(ifc2, cls2)) = (
              case Interface.compare(ifc1, ifc2)
               of EQUAL => Class.compare(cls1, cls2)
                | order => order
              (* end case *))
          | compare (IfcCls _, _) = LESS
          | compare (_, IfcCls _) = GREATER
          | compare (IfcIfc(ifc11, ifc12), IfcIfc(ifc21, ifc22)) = (
              case Interface.compare(ifc11, ifc21)
               of EQUAL => Interface.compare(ifc12, ifc22)
                | order => order
              (* end case *))
      end)

  end
