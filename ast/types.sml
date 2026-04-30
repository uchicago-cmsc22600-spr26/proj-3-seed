(* types.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Types =
  struct

    datatype t
      = OptTy of t                      (* optional (?) type *)
      | ClsTy of class_tyc              (* class type constructor *)
      | IfcTy of interface_tyc          (* interface type constructor *)
      | PrimTy of prim_tyc              (* primitive type *)
      | VoidTy                          (* void type (for return types only) *)
      | ErrorTy                         (* error type; we use this type to allow error
                                         * recovory without cascading errors.
                                         *)

  (* a class type constructor; represented as a reference to a record
   * describing the signature of the class.  See the `Class` structure
   * for operations on this representation.
   *)
    and class_tyc = ClsTyc of {
        name : Atom.atom,               (* the class name *)
        prim : bool,                    (* true for internal classes (e.g., Basis.boolCls) *)
        params : t list,                (* the types of the class's parameters *)
        super : class_tyc option,       (* optional super class *)
        vars : memb_var list,           (* list of member variables in canonical order *)
        funs : memb_fun list            (* list of member functions in canonical order *)
      } ref

  (* an interface type constructor; represented as a reference to a record
   * describing the signature of the class.  See the `Interface` structure
   * for operations on this representation.
   *)
    and interface_tyc = IfcTyc of {
        name : Atom.atom,               (* the interface name *)
        funs : memb_fun list            (* list of member functions in canonical order *)
      } ref

  (* primitive types *)
    and prim_tyc = BoolTy | IntTy | StringTy

  (* a member-function signature *)
    and fun_sig = FUNTY of t list * t

  (* a class/interface member function *)
    and memb_fun = MFun of {
        name : Atom.atom,               (* the function's name *)
        sign : fun_sig                  (* the function's type *)
      }

  (* a class member variable *)
    and memb_var = MVar of {
        name : Atom.atom,               (* the variable's name *)
        typ : t                         (* the variable's type *)
      }

  (* primitive types *)
    val boolTy = PrimTy BoolTy
    val intTy = PrimTy IntTy
    val stringTy = PrimTy StringTy

    fun toString (OptTy ty) = toString ty ^ "?"
      | toString (ClsTy(ClsTyc(ref{name, ...}))) = Atom.toString name
      | toString (IfcTy(IfcTyc(ref{name, ...}))) = Atom.toString name
      | toString (PrimTy BoolTy) = "bool"
      | toString (PrimTy IntTy) = "int"
      | toString (PrimTy StringTy) = "string"
      | toString VoidTy = "void"
      | toString ErrorTy = "<**error**>"

  end
