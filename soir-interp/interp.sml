(* interp.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * The main function for the SOIR interpreter.  The interpreter recognizes the following
 * command-line options:
 *
 *      --trace                 output an execution trace to the terminal
 *      --print <file>          specify an output file for the program's output
 *      --step-limit <n>        limit the execution to at most <n> steps
 *      --depth-limit <n>       limit the call depth to at most <n> calls
 *)

structure Interp : sig

    val main : string * string list -> OS.Process.status

  end = struct

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm =
          if Error.anyErrors errStrm
            then raise Error.ERROR
            else ()

  (* check that an input file exists.  If it does not exist, then print an error message
   * and raise the ERROR exception.
   *)
    fun doesFileExist filename = if OS.FileSys.access(filename, [OS.FileSys.A_READ])
          then ()
          else (
            err(concat["source file \"", filename, "\" does not exist or is not readable\n"]);
            raise Error.ERROR)

  (* dump the SOIR to a file *)
    fun dumpSOIR (errStrm, base, prog) = let
          val outFile = OS.Path.joinBaseExt{base = base, ext = SOME "ir"}
          val outS = TextIO.openOut outFile
          in
            SOIRPP.output (outS, "after normalization", prog);
            TextIO.closeOut outS
          end

  (* process an input file *)
    fun doFile (errStrm, filename) = let
        (* check that the input file exists *)
          val _ = doesFileExist filename
          val base = OS.Path.base filename
        (* parse the input file *)
          val parseTree = let
                val inS = TextIO.openIn filename
                val pt = Parser.parse (inS, errStrm)
                in
                  TextIO.closeIn inS;
                  checkForErrors errStrm;
                  valOf pt
                end
        (* type check the program *)
          val ast = TypeChecker.check (errStrm, parseTree)
          val _ = checkForErrors errStrm
        (* convert to SOIR *)
          val soir = Normalize.transform ast
          in
            dumpSOIR (errStrm, base, soir);
            soir
          end

  (* run the interpreter on the the program *)
    fun run (ctls, program as SOIR.PROGRAM{main=SOIR.FuncDef{f, ...}, ...}) = let
          val env = DynamicEnv.new (ctls, program)
          in
            case Eval.apply (env, f, [])
             of Value.VOID => ()
              | v => Runtime.failure(concat[
                    "unexpected value ", Value.toString v, "returned from program"
                  ])
            (* end case *)
          end

    fun handleExn Error.ERROR = OS.Process.failure
      | handleExn exn = (
          err (concat [
              "uncaught exception ", General.exnName exn,
              " [", General.exnMessage exn, "]\n"
            ]);
          List.app (fn s => err (concat ["  raised at ", s, "\n"])) (SMLofNJ.exnHistory exn);
          OS.Process.failure)

    fun usage () = (
          TextIO.output(TextIO.stdErr, String.concat[
              "usage: ", CommandLine.name(),
              " [--trace] [--step-limit=<n>] [--depth-limit=<n>] <file>\n"
            ]);
          OS.Process.exit OS.Process.failure)

    fun getNum n = (case Int.fromString n
           of SOME n => n
            | _ => usage()
          (* end case *))

  (* process the command-line options *)
    fun doOptions (_, _, _, _, []) = usage ()
      | doOptions (trace, outF, sl, dl, "--print"::file::args) =
          doOptions(trace, SOME file, sl, dl, args)
      | doOptions (trace, outF, sl, dl, "--trace"::args) =
          doOptions(true, outF, sl, dl, args)
      | doOptions (trace, outF, sl, dl, "--step-limit"::n::args) =
          doOptions(trace, outF, getNum n, dl, args)
      | doOptions (trace, outF, sl, dl, "--depth-limit"::n::args) =
          doOptions(trace, outF, sl, getNum n, args)
      | doOptions (trace, outF, sl, dl, arg::args) =
          if String.isPrefix "--" arg orelse not(List.null args)
            then usage()
            else ({trace = trace, stepLimit = sl, depthLimit = dl}, outF, arg)

    fun main (_, args) = let
          val (ctls, outF, filename) = doOptions (false, NONE, 500, 20, args)
          val _ = (case outF
                 of SOME file => let
                      val outS = TextIO.openOut file
                      in
                        Runtime.outStrm := SOME outS
                      end
                  | NONE => ()
                (* end case *))
          fun close () = (case !Runtime.outStrm
                 of SOME outS => TextIO.closeOut outS
                  | NONE => ()
                (* end case *))
          val errStrm = Error.mkErrStream filename
          fun finish () = (
                close ();
                if Error.anyErrors errStrm
                  then (Error.report (TextIO.stdErr, errStrm); OS.Process.failure)
                  else (
                    if Error.anyWarnings errStrm
                      then Error.report (TextIO.stdErr, errStrm)
                      else ();
                    OS.Process.success))
          in
            (run (ctls, doFile (errStrm, filename)); finish())
                handle ex => (ignore (finish()); handleExn ex);
            OS.Process.success
          end

  end
