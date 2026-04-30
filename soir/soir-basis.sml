(* soir-basis.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Various objects and names in the runtime system represented in SOIR.
 *)

structure SOIRBasis : sig

  (* runtime functions for boolI support *)
    val boolToStringFunc : SOIRFunc.t           (* convert to string *)

  (* runtime functions for intI support *)
    val intToStringFunc : SOIRFunc.t            (* convert to string *)
    val intCharFunc : SOIRFunc.t                (* convert to character *)

  (* runtime functions for stringI support *)
    val stringCharAtFunc : SOIRFunc.t           (* get character code at index *)
    val stringLengthFunc : SOIRFunc.t           (* string length *)
    val stringSubstringFunc : SOIRFunc.t        (* get substring *)
    val stringToIntFunc : SOIRFunc.t            (* convert to integer *)

  (* runtime functions for systemI support *)
    val exitFunc : SOIRFunc.t                   (* exit program *)
    val inputFunc : SOIRFunc.t                  (* get input *)

  (* other runtime functions *)
    val failFunc : SOIRFunc.t                   (* report failure and quit *)
    val printFunc : SOIRFunc.t                  (* print output *)
    val stringCatFunc : SOIRFunc.t              (* concatenate two strings *)
    val instanceOfFunc : SOIRFunc.t                 (* class instance test (used to
                                                 * implement downcasts)
                                                 *)

  (* global system variable *)
    val systemVar : SOIRVar.t

  (* the main function that the runtime system will call *)
    val soolMain : SOIRFunc.t

  end = struct

    structure IR = SOIR

    val boolToStringFunc = SOIRFunc.new' ("_sool_bool_to_string", [IR.IntTy], IR.StringTy)

    val intToStringFunc = SOIRFunc.new' ("_sool_int_to_string", [IR.IntTy], IR.StringTy)
    val intCharFunc = SOIRFunc.new' ("_sool_int_to_char", [IR.StringTy], IR.StringTy)

    val stringCharAtFunc = SOIRFunc.new' ("_sool_char_at", [IR.StringTy, IR.IntTy], IR.IntTy)
    val stringLengthFunc = SOIRFunc.new' ("_sool_string_length", [IR.StringTy], IR.IntTy)
    val stringSubstringFunc =
          SOIRFunc.new' ("_sool_substring", [IR.StringTy, IR.IntTy, IR.IntTy], IR.StringTy)
    val stringToIntFunc = SOIRFunc.new' ("_sool_string_to_int", [IR.StringTy], IR.IntTy)

    val exitFunc = SOIRFunc.new' ("_sool_exit", [], IR.VoidTy)
    val inputFunc = SOIRFunc.new' ("_sool_input", [], IR.OptionTy IR.StringTy)

    val failFunc = SOIRFunc.new' ("_sool_fail", [IR.StringTy], IR.VoidTy)
    val printFunc = SOIRFunc.new' ("_sool_print", [IR.StringTy], IR.VoidTy)
    val stringCatFunc = SOIRFunc.new' ("_sool_string_cat", [IR.StringTy, IR.StringTy], IR.StringTy)
    val instanceOfFunc = SOIRFunc.new' ("_sool_instance_of", [IR.ObjectTy, IR.ClassTy], IR.BoolTy)

    val systemVar = SOIRVar.new ("_sool_system", IR.ObjectTy)

    val soolMain = SOIRFunc.new' ("_sool_main", [], IR.VoidTy)

  end
