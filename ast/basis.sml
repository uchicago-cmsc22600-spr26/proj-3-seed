(* basis.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Representations of various interfaces and classes in the SooL Basis.
 *)

structure Basis : sig

  (* interfaces for primitive types *)
    val toStringI : Interface.t
    val boolI : Interface.t
    val intI : Interface.t
    val stringI : Interface.t
    val systemI : Interface.t

  (* internal classes for the primitive types; these are used during the normalization
   * phase to represent primitive types in the coercion graph.
   *)
    val boolCls : Class.t
    val intCls : Class.t
    val stringCls : Class.t

  (* the internal class of the system object *)
    val systemCls : Class.t

  (* the global "system" variable *)
    val systemVar : LocalVar.t

  (* the internal "mainI" interface is used to check the user's Main class
   * implements a run function
   *)
    val mainI : Interface.t

  (* the run method for the mainI interface *)
    val runFun : MemberFun.t

  (* the print function for the systemI interface (and the internal system class) *)
    val systemPrintFun : MemberFun.t

  end = struct

    structure Ty = Types

  (* basis names *)
    val a_boolI = Atom.atom "boolI"
    val a_char = Atom.atom "char"
    val a_charAt = Atom.atom "charAt"
    val a_exit = Atom.atom "exit"
    val a_fail = Atom.atom "fail"
    val a_input = Atom.atom "input"
    val a_intI = Atom.atom "intI"
    val a_length = Atom.atom "length"
    val a_not = Atom.atom "not"
    val a_print = Atom.atom "print"
    val a_stringI = Atom.atom "stringI"
    val a_substring = Atom.atom "substring"
    val a_system = Atom.atom "system"
    val a_systemI = Atom.atom "systemI"
    val a_toInt = Atom.atom "toInt"
    val a_toString = Atom.atom "toString"
    val a_toStringI = Atom.atom "toStringI"

    fun mfun (id, paramTys, retTy) = Ty.MFun{name = id, sign = Ty.FUNTY(paramTys, retTy)}

    val runFun = mfun(Atom.atom "run", [], Ty.VoidTy)

  (* basis interface types *)
    local
      val toStringFn = mfun(a_toString, [], Ty.stringTy)
      fun mkIfc (name, fns) = let
            val ifc = Interface.new name
            in
              Interface.init (ifc, fns);
              ifc
            end
    in
    val toStringI = mkIfc(a_toStringI, [toStringFn])
    val boolI = mkIfc(a_boolI, [toStringFn, mfun(a_not, [], Ty.boolTy)])
    val intI = mkIfc(a_intI, [toStringFn, mfun(a_char, [], Ty.stringTy)])
    val stringI = mkIfc(a_stringI, [
            toStringFn,
            mfun(a_length, [], Ty.intTy),
            mfun(a_substring, [Ty.intTy, Ty.intTy], Ty.stringTy),
            mfun(a_charAt, [Ty.intTy], Ty.intTy),
            mfun(a_toInt, [], Ty.OptTy Ty.intTy)
          ])
    val systemPrintFun = mfun(a_print, [Ty.IfcTy toStringI], Ty.VoidTy)
    val systemI = mkIfc (a_systemI, [
            systemPrintFun,
            mfun(a_input, [], Ty.OptTy Ty.stringTy),
            mfun(a_exit, [], Ty.VoidTy),
            mfun(a_fail, [Ty.stringTy], Ty.VoidTy)
          ])
  (* the "mainI" interface is used to check the user's Main class implements a run function *)
    val mainI = mkIfc (Atom.atom "__MainI__", [runFun])
    end (* local *)

  (* internal classes for the primitive types. *)
    local
      fun mk (name, mvs, ifc) = let
            val cls = Ty.ClsTyc(ref{
                    name = Atom.atom name, prim = true, params = [], super = NONE,
                    vars = [], funs = []
                  })
            val cls = Class.new (Atom.atom name, NONE)
            in
              Class.init (cls, List.map MemberVar.typeOf mvs, mvs, Interface.funsOf ifc);
              cls
            end
      fun mkPrimClass (name, ty, ifc) = mk (name, [Ty.MVar{name=Atom.atom "value", typ = ty}], ifc)
      fun mkPrimClass' (name, ifc) = mk (name, [], ifc)
    in
    val boolCls = mkPrimClass ("__Bool__", Ty.boolTy, boolI)
    val intCls = mkPrimClass ("__Int__", Ty.intTy, intI)
    val stringCls = mkPrimClass ("__String__", Ty.stringTy, stringI)
    val systemCls = mkPrimClass' ("__System__", systemI)
    end

  (* the global system variable.  Note that we give this variable the
   * internal system-class type, instead of the visible systemI type
   * so that dispatching system functions is more efficient.
   *)
    val systemVar = LocalVar.new (a_system, Ty.ClsTy(systemCls))

  end
