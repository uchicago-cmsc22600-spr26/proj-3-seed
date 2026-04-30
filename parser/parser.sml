(* parser.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Combine the generated parser and lexer to implement the parser.
 *)

structure Parser : sig

  (* parse an Sool program from an input stream using the given error stream to record errors *)
    val parse : TextIO.instream * Error.err_stream -> ParseTree.program option

  (* `lexer (inStrm, errStrm, outStrm)`
   * tests scanner by printing tokens from `inStrm` to the given output stream
   *)
    val lexer : TextIO.instream * Error.err_stream * TextIO.outstream -> unit

  end = struct

    structure Tok = SoolTokens

  (* glue together the lexer and parser *)
    structure SoolParser = SoolParseFn(SoolLex)

  (* error function for lexers *)
    fun lexErr errStrm (span, msg) = Error.errorAt(errStrm, span, msg)

  (* to get ADD and DEL tokens *)
    datatype add_or_delete = datatype AntlrRepair.add_or_delete

  (* map tokens to strings *)
    fun tokToString ADD (Tok.ID _) = "<identifier>"
      | tokToString DEL (Tok.ID x) = Atom.toString x
      | tokToString ADD (Tok.NUMBER _) = "<number>"
      | tokToString DEL (Tok.NUMBER i) = IntInf.toString i
      | tokToString ADD (Tok.STRING _) = "<string>"
      | tokToString DEL (Tok.STRING s) = concat["\"", String.toString s, "\""]
      | tokToString ADD Tok.KW_bool = "<type>"
      | tokToString ADD Tok.KW_int = "<type>"
      | tokToString ADD Tok.KW_string = "<type>"
      | tokToString _ tok = Tok.toString tok

  (* error function for parsers *)
    val parseErr = Error.parseError tokToString

  (* parse a file, returning a parse tree *)
    fun parse (inStrm, errStrm) = let
          fun get () = TextIO.input inStrm
          val lexer = SoolLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
          val (yield, _, errs) = SoolParser.parse lexer (SoolLex.streamify get)
          in
            List.app (parseErr errStrm) errs;
            yield
          end

  (* test scanner by printing tokens from `inStrm` to the given output stream *)
    fun lexer (inStrm, errStrm, outStrm) = let
          val lex = SoolLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
          fun get strm = let
                val (tok, span, strm) = lex strm
                val loc = Error.location (errStrm, span)
                in
                  TextIO.output(outStrm, concat[
                      StringCvt.padRight #" " 23 (SoolTokens.toString tok), " ",
                      Error.locToString loc, "\n"
                    ]);
                  case tok
                   of Tok.EOF => ()
                    | _ => get strm
                  (* end case *)
                end
          in
            get (SoolLex.streamify (fn () => TextIO.input inStrm))
          end

  end
