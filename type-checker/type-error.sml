(* type-error.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Support for reporting type error messages.
 *)

structure TypeError : sig

  (* pieces of an error message *)
    datatype token
      = S of string             (* literal string *)
      | A of Atom.atom          (* atom; will print in single quotes ('...') *)
      | V of LocalVar.t         (* local variable; will print name in single quotes ('...') *)
      | TY of Types.t           (* type expression *)
      | TYS of Types.t list     (* list of types *)
      | LN of Error.location    (* source-code location; prints as a line number *)

  (* format an error message *)
    val format : token list -> string list

  (* record a warning message *)
    val warning : Env.t * token list -> unit

  (* record an error message *)
    val error  : Env.t * token list -> unit

  end = struct

    structure Ty = Types

    datatype token
      = S of string | A of Atom.atom | V of LocalVar.t
      | TY of Ty.t | TYS of Ty.t list | LN of Error.location

    (* convert a list of types to a string.  Note, that `concatWithMap` is an SML Basis
     * Library extension that was implemented in SML/NJ 110.79.
     *)
      fun tysToString tys = String.concat[
              "(", String.concatWithMap ", " Ty.toString tys, ")"
            ]

      fun tok2str (S s) = s
        | tok2str (A a) = concat["'", Atom.toString a, "'"]
        | tok2str (V x) = concat["'", LocalVar.nameOf x, "'"]
        | tok2str (TY ty) = Ty.toString ty
        | tok2str (TYS tys) = tysToString tys
        | tok2str (LN loc) = Error.fmt ("line %l", "<unknown>") loc

    fun format toks = List.map tok2str toks

    fun message reportFn (env, toks) = let
          val (errStrm, span) = Env.context env
          in
            reportFn (errStrm, span, List.map tok2str toks)
          end

    val warning = message Error.warningAt
    val error = message Error.errorAt

  end
