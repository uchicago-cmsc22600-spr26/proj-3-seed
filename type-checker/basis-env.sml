(* basis-env.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * The SooL Basis environment.
 *)

structure BasisEnv : sig

  (* initial variable environment (contains "system" variable) *)
    val varEnv : LocalVar.t AtomMap.map

  (* initial class environment (empty) *)
    val classEnv : unit -> Types.class_tyc AtomTable.hash_table

  (* initial interface environment (systemI, toStringI, ...) *)
    val interfaceEnv : unit -> Types.interface_tyc AtomTable.hash_table

  (* return the type of a binary operator *)
    val typeOfBinOp : BinOp.t -> Types.t * Types.t * Types.t

  (* return the interface of a primitive type *)
    val interfaceOf : Types.prim_tyc -> Types.interface_tyc

  end = struct

    structure Ty = Types
    structure B = Basis
    structure AMap = AtomMap
    structure ATbl = AtomTable

  (* basis names *)
    val a_boolI = Atom.atom "boolI"
    val a_intI = Atom.atom "intI"
    val a_stringI = Atom.atom "stringI"
    val a_system = Atom.atom "system"
    val a_systemI = Atom.atom "systemI"
    val a_toStringI = Atom.atom "toStringI"

    val varEnv = AtomMap.singleton (a_system, B.systemVar)

    fun classEnv () = AtomTable.mkTable (32, Fail "classEnv")

    fun interfaceEnv () = let
          val tbl = AtomTable.mkTable (32, Fail "interfaceEnv")
          in
            List.app (AtomTable.insert tbl) [
                (a_boolI,       B.boolI),
                (a_intI,        B.intI),
                (a_stringI,     B.stringI),
                (a_systemI,     B.systemI),
                (a_toStringI,   B.toStringI)
              ];
            tbl
          end

    fun typeOfBinOp rator = (case rator
           of BinOp.^<^ => (Ty.intTy, Ty.intTy, Ty.boolTy)
            | BinOp.^<=^ => (Ty.intTy, Ty.intTy, Ty.boolTy)
            | BinOp.^@^ => (Ty.stringTy, Ty.stringTy, Ty.stringTy)
            | BinOp.^+^ => (Ty.intTy, Ty.intTy, Ty.intTy)
            | BinOp.^-^ => (Ty.intTy, Ty.intTy, Ty.intTy)
            | BinOp.^*^ => (Ty.intTy, Ty.intTy, Ty.intTy)
            | BinOp.^/^ => (Ty.intTy, Ty.intTy, Ty.intTy)
            | _ => raise Fail "typeOfBinOp: not defined for equality ops"
          (* end case *))

    fun interfaceOf Ty.BoolTy = B.boolI
      | interfaceOf Ty.IntTy = B.intI
      | interfaceOf Ty.StringTy = B.stringI

  end
