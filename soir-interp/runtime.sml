(* runtime.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Runtime : sig

    val outStrm : TextIO.outstream option ref

    val failure : string -> 'a

  (* runtime function for reporting failures *)
    val failFunc : Value.t list -> 'a

  (* runtime function for printing *)
    val printFunc : Value.t list -> Value.t

  (* runtime function for string concatenation *)
    val stringCat : Value.t list -> Value.t

  (* runtime function for dynamic type tests *)
    val instanceOf : Value.t list -> Value.t

  (* global system variable *)
    val systemVar : SOIR.class -> Value.t

  (* primitive functions for the internal boolean class *)
    val bool_toString : Value.t list -> Value.t
    val bool_not : Value.t list -> Value.t

  (* primitive functions for the internal integer class *)
    val int_toString : Value.t list -> Value.t
    val int_char : Value.t list -> Value.t

  (* primitive functions for the internal integer class *)
    val string_toString : Value.t list -> Value.t
    val string_length : Value.t list -> Value.t
    val string_substring : Value.t list -> Value.t
    val string_charAt : Value.t list -> Value.t
    val string_toInt : Value.t list -> Value.t

  (* primitive functions for the internal system class *)
    val system_input : Value.t list -> Value.t
    val system_exit : Value.t list -> Value.t
    val system_fail : Value.t list -> Value.t

  end = struct

    structure IR = SOIR
    structure V = Value

    val outStrm : TextIO.outstream option ref = ref NONE

    fun stdPrint s = (case !outStrm
           of NONE => print s
            | SOME outS => TextIO.output(outS, s)
          (* end case *))

    fun errPrint s = (case !outStrm
           of NONE => TextIO.output(TextIO.stdErr, s)
            | SOME outS => TextIO.output(outS, s)
          (* end case *))

    fun failure msg = (
          errPrint (concat["FAILURE: ", msg, "\n"]);
          OS.Process.exit OS.Process.failure)

  (* wrapper around primitive functions; catches exceptions (such as Match) and
   * reports them as fatal errors.
   *)
    fun wrap name f args = (f args handle ex => failure(concat[
              "error in ", name, "(", String.concatWithMap "," V.toString args, ")"
            ]))

    fun meth name f (args as (V.OBJ self :: rest)) =
          (f (Array.sub(self, 1) :: rest) handle ex => failure(concat[
              "error in ", name, "(", String.concatWithMap "," V.toString args, ")"
            ]))
      | meth name f args =
          failure(concat["error in ", name, "(", String.concatWithMap "," V.toString args, ")"])

    fun mkINT n = V.INT(IntInf.fromInt n)

  (* primitive functions for the internal boolean class *)
    val bool_toString =
          meth "bool.toString" (fn [V.BOOL b] => V.STR(Bool.toString b))
    val bool_not =
          meth "bool.not" (fn [V.BOOL b] => V.BOOL(not b))

  (* primitive functions for the internal integer class *)
    val int_toString =
          meth "int.toString" (fn [V.INT n] => V.STR(Format.format "%d" [Format.LINT n]))
    val int_char =
          meth "int.char" (fn [V.INT n] => V.STR(String.str(Char.chr(Int.fromLarge n))))

  (* primitive functions for the internal integer class *)
    val string_toString =
          meth "string.toString" (fn [V.STR s] => V.STR s)
    val string_length =
          meth "string.length" (fn [V.STR s] => mkINT(size s))
    val string_substring =
          meth "string.substring"
            (fn [V.STR s, V.INT i, V.INT n] => V.STR(substring(s, Int.fromLarge i, Int.fromLarge n)))
    val string_charAt =
          meth "string.charAt"
            (fn [V.STR s, V.INT i] => mkINT(Char.ord(String.sub(s, Int.fromLarge i))))
    val string_toInt =
          meth "string.toInt"
            (fn [V.STR s] => (case IntInf.fromString s of SOME n => V.INT n | NONE => V.NIL))

  (* primitive functions for the internal system class *)
    val system_input =
          wrap "system.input"
            (fn [V.OBJ _] => case TextIO.inputLine TextIO.stdIn of SOME s => V.STR s | _ => V.NIL)
    fun system_exit args =
          wrap "system.exit" (fn [V.OBJ _] => OS.Process.exit OS.Process.success) args
    fun system_fail args =
          wrap "system.fail" (fn [V.OBJ _, V.STR s] => failure s) args

    val stringCat = wrap "string_cat" (fn [V.STR s1, V.STR s2] => V.STR(s1 ^ s2))

    val instanceOf =
          wrap "instance_of" (fn [V.OBJ obj, V.META(IR.CLASS{tyc=targetCls, ...})] => let
            val V.META objCls = Array.sub(obj, 0)
            fun search (objCls as IR.CLASS{tyc, super, ...}) =
                  if Class.same(tyc, targetCls)
                    then V.BOOL true
                    else (case super
                       of SOME superCls => search superCls
                        | NONE => V.BOOL false
                      (* end case *))
            in
              search objCls
            end)

    fun failFunc args = wrap "fail" (fn [V.STR msg] => failure msg) args

    val printFunc = wrap "print" (fn [V.STR msg] => (stdPrint msg; V.VOID))

    fun systemVar sysCls = V.OBJ(Array.array(1, V.META sysCls))

  end
