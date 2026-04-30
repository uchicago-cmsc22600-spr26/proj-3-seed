(* sool.lex
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * ml-ulex specification for SooL
 *)

%name SoolLex;

%arg (lexErr);

%defs(

    structure T = SoolTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* starting position of current block comment or string; used for error reporting *)
    val startPos : Error.pos ref = ref 0

  (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

  (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

  (* make a string from buf *)
    fun mkString () = (T.STRING(String.concat(List.rev(!buf))) before buf := [])

  (* keyword lookup table *)
    local
      val find =
          let val tbl = AtomTable.mkTable (32, Fail "keywords")
              fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
          in
              app ins [
                ("as",          T.KW_as),
                ("bool",        T.KW_bool),
                ("class",       T.KW_class),
                ("else",        T.KW_else),
                ("extends",     T.KW_extends),
                ("false",       T.KW_false),
                ("if",          T.KW_if),
                ("int",         T.KW_int),
                ("interface",   T.KW_interface),
                ("method",      T.KW_method),
                ("nil",         T.KW_nil),
                ("override",    T.KW_override),
                ("return",      T.KW_return),
                ("self",        T.KW_self),
                ("string",      T.KW_string),
                ("true",        T.KW_true),
                ("var",         T.KW_var),
                ("void",        T.KW_void),
                ("while",       T.KW_while)
              ];
              AtomTable.find tbl
          end
    in
  (* return either a keyword token or an ID token *)
    fun idToken id = let
          val ida = Atom.atom id
          in
            case find ida
             of NONE => T.ID ida
              | SOME kw => kw
            (* end case *)
          end
    end
);

%states INITIAL COM STRING;

%let letter = [a-zA-Z];
%let digit = [0-9];
%let idchar = {letter}|{digit}|"_";
%let id = {letter}{idchar}*;
%let esc = "\\"[nrt\\\"]|"\\"{digit}{digit}{digit};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let ws = " "|[\t\n\v\f\r];
%let eol = "\n"|"\r\n"|"\r";

<INITIAL> "/*"          => (YYBEGIN COM; startPos := yypos; skip());
<INITIAL> "("           => (T.LP);
<INITIAL> ")"           => (T.RP);
<INITIAL> "{"           => (T.LCB);
<INITIAL> "}"           => (T.RCB);
<INITIAL> ":"           => (T.COLON);
<INITIAL> "="           => (T.EQ);
<INITIAL> ","           => (T.COMMA);
<INITIAL> ";"           => (T.SEMI);
<INITIAL> "->"          => (T.ARROW);
<INITIAL> "."           => (T.DOT);
<INITIAL> "!"           => (T.BANG);
<INITIAL> "?"           => (T.QMARK);
<INITIAL> "??"          => (T.DQMARK);
<INITIAL> "||"          => (T.ORELSE);
<INITIAL> "&&"          => (T.ANDALSO);
<INITIAL> "<="          => (T.LTE);
<INITIAL> "<"           => (T.LT);
<INITIAL> "=="          => (T.EQEQ);
<INITIAL> "!="          => (T.NEQ);
<INITIAL> "+"           => (T.PLUS);
<INITIAL> "-"           => (T.MINUS);
<INITIAL> "@"           => (T.CONCAT);
<INITIAL> "*"           => (T.TIMES);
<INITIAL> "/"           => (T.DIV);
<INITIAL> {id}          => (idToken yytext);
<INITIAL> {digit}+      => (T.NUMBER(valOf (IntInf.fromString yytext)));
<INITIAL> {ws}          => (skip ());
<INITIAL> "//"[^\n\r]*{eol}  (* end-of-line comment *)
                        => (skip ());
<INITIAL> "\""          => (YYBEGIN STRING; startPos := yypos; continue());
<INITIAL>.              => (lexErr((yypos, yypos), ["bad character `", String.toString yytext, "'"]);
                            continue());
<INITIAL> <<EOF>>       => (T.EOF);

(* block-comment lexing *)
<COM> "*/"              => (YYBEGIN INITIAL; skip());
<COM> <<EOF>>           => (lexErr((!startPos, yypos), ["unclosed comment at end of file"]);
                            T.EOF);
<COM> .                 => (skip());

(* string lexing *)
<STRING> {esc}          => (if (yytext = "\\000")
                              then lexErr((yypos, yypos), [
                                  "illegal escape '\\000' in string literal"
                                ])
                              else addStr(valOf(String.fromString yytext));
                            continue());
<STRING> {sgood}+       => (addStr yytext; continue());
<STRING> "\""           => (YYBEGIN INITIAL; mkString());
<STRING> "\\".          => (lexErr((yypos, yypos), [
                                "bad escape character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue());
<STRING> \n|\r|\r\n     => (lexErr((!startPos, yypos), ["unclosed string at end of line"]);
                            YYBEGIN INITIAL; mkString());
<STRING> .              => (lexErr((yypos, yypos), [
                                "bad character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue());
<STRING> <<EOF>>        => (lexErr((!startPos, yypos), ["unclosed string at end of file"]);
                            T.EOF);
