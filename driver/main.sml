(* main.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Driver for Project 3.
 *)

structure Main : sig

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

  (* dump the parse tree to a file *)
    fun dumpPT (errStrm, base, marks, prog) = let
          val outFile = OS.Path.joinBaseExt{base = base, ext = SOME "pt"}
          val outS = TextIO.openOut outFile
          in
            PrintParseTree.program (
              PrintParseTree.new{outS=outS, errS=errStrm, showMarks=marks},
              prog);
            TextIO.closeOut outS
          end

  (* dump the abstract syntax tree to a file *)
    fun dumpAST (base, prog) = let
          val outFile = OS.Path.joinBaseExt{base = base, ext = SOME "ast"}
          val outS = TextIO.openOut outFile
          in
            ASTPP.output (outS, "after type checking", prog);
            TextIO.closeOut outS
          end

  (* dump the SOIR to a file *)
    fun dumpSOIR (base, prog, msg, ext) = let
          val outFile = OS.Path.joinBaseExt{base = base, ext = SOME ext}
          val outS = TextIO.openOut outFile
          in
            SOIRPP.output (outS, msg, prog);
            TextIO.closeOut outS
          end

  (* process an input file *)
    fun doFile (errStrm, filename, optimize) = let
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
        (* output parse tree *)
          val _ = dumpPT (errStrm, base, false, parseTree)
        (* type check the program *)
          val ast = TypeChecker.check (errStrm, parseTree)
          val _ = checkForErrors errStrm
        (* output AST *)
          val _ = dumpAST (base, ast)
        (* convert to SOIR *)
          val soir = Normalize.transform ast
          in
            dumpSOIR (base, soir, "after normalization", "ir")
          end

    fun handleExn Error.ERROR = OS.Process.failure
      | handleExn exn = (
          err (concat [
              "uncaught exception ", General.exnName exn,
              " [", General.exnMessage exn, "]\n"
            ]);
          List.app (fn s => err (concat ["  raised at ", s, "\n"])) (SMLofNJ.exnHistory exn);
          OS.Process.failure)

    fun usage () = TextIO.output(TextIO.stdErr, concat[
              "usage: ", CommandLine.name(), " [--test-scanner | -O] <file>\n"
            ]);

  (* process command-line options *)
    fun doOpts opts = let
          fun doOpts ("-O"::opts, scanTst, _) = doOpts(opts, scanTst, true)
            | doOpts ("--test-scanner"::opts, _, optimize) = doOpts (opts, true, optimize)
            | doOpts ([file], scanTst, optimize) = {
                  file = file, testScanner = scanTst, optimize = optimize
                }
            | doOpts _ = (usage (); OS.Process.exit OS.Process.failure)
          in
            doOpts (opts, false, false)
          end

    fun finish errStrm = if Error.anyErrors errStrm
          then (Error.report (TextIO.stdErr, errStrm); OS.Process.failure)
          else (
            if Error.anyWarnings errStrm
              then Error.report (TextIO.stdErr, errStrm)
              else ();
            OS.Process.success)

    fun main (_, opts) = (case doOpts opts
           of {testScanner=true, optimize=true, ...} => (usage(); OS.Process.failure)
            | {file, testScanner=true, ...} => let
              (* scan the input and print the tokens to <file>.toks *)
                val errStrm = Error.mkErrStream file
                val base = OS.Path.base file
                val outFile = OS.Path.joinBaseExt{base = base, ext = SOME "toks"}
                val inS = TextIO.openIn file
                val outS = TextIO.openOut outFile
                fun cleanup () = (TextIO.closeIn inS; TextIO.closeOut outS)
                in
                  (Parser.lexer (inS, errStrm, outS); cleanup(); finish errStrm)
                    handle ex => (cleanup(); ignore (finish errStrm); handleExn ex)
                end
            | {file, optimize, ...} => let
              (* parse the input and print the parse tree to <file>.pt *)
                val errStrm = Error.mkErrStream file
                in
                  (doFile (errStrm, file, optimize); finish errStrm)
                    handle ex => (ignore (finish errStrm); handleExn ex)
                end
          (* end case *))

  end
