{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)
}

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

let identchar = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let decimal_escape = '\\' digit digit digit
let hexa_escape = "\\0" ("x" | "X") hexa hexa
let simple_escape = '\\' ['n' 't' 'b' 'r' '\\' '\'' '"']
let printable = ['\032'-'\126']

let atom = decimal_escape | hexa_escape | simple_escape | printable

rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }

  (** Commentaires *)
  | "##" [^ '\n']*   { token lexbuf }
  | "{*"            { comment 1 lexbuf}

  (** Mots-clés *)
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "while"         { WHILE }
  | "do"            { DO }
  | "until"         { UNTIL }
  | "for"           { FOR }
  | "from"          { FROM }
  | "to"            { TO }
  | "match"         { MATCH }
  | "fun"           { FUN }
  | "let"           { LET }
  | "type"          { TYPE }
  | "extern"        { EXTERN }
  | "ref"           { REF }

  (** Identificateurs *)
  | "`" lower identchar* as id   { TYPE_VARIABLE id }
  | lower identchar* as id       { IDENT id }
  | upper identchar* as id       { CONSTR id }

  (** Littéraux entiers *)
  | (('-'? digit+) | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ | ("0b" | "0B") ['0'-'1']+ | "0o" ['0'-'7']+) as n
    {
      try INT (Mint.of_string n)
      (* TODO: Mint.DoesNotFit *)
      with Failure _ -> error lexbuf "Integer literal too large."
    }

  (** caractères et chaines *)
  (** Atom  *)
  | "'\\n'"         { CHAR '\n' }
  | "'\\t'"         { CHAR '\t' }
  | "'\\b'"         { CHAR '\b' }
  | "'\\r'"         { CHAR '\r' }
  | "'\\\\'"        { CHAR '\\' }
  | "'\\''"         { CHAR '\'' }
  | "'\\" (digit digit digit as i) "'" {
      let c = int_of_string i in
      if c < 0 || c > 255
      then error lexbuf "Invalid decimal escape code."
      else CHAR (char_of_int c)
    }
  | "'\\0" ("x" | "X") (hexa hexa as i) "'" {
      let c = int_of_string ("0x" ^ i) in
      if c < 0 || c > 255
      then error lexbuf "Invalid hexadecimal escape code."
      else CHAR (char_of_int c)
    }
  | "'\\0" ("b" | "B") (['0'-'1']+ as i) "'" {
      let c = int_of_string ("0b" ^ i) in
      if c < 0 || c > 255
      then error lexbuf "Invalid binary escape code." 
      else CHAR (char_of_int c)
    }

  | ('\'' ((printable # '\'') as c) '\'') { CHAR c }

  (* TODO: rework "..." so that we can verify if an opened '"' is closed *)
  | '"' ( atom | "'" | "\"" # '"' )* '"' as s { STRING s }

    (** Ponctuation *)
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "!"             { EXL }
  | "."             { DOT }
  | "["             { LCROCHET }
  | "]"             { RCROCHET }
  | "|"             { BAR }
  | ","             { COM }
  | ";"             { SEMI }
  | ":"             { DOUBLE_POINT }
  | "="             { EQ }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { STAR }
  | "/"             { DIV }
  | "\\"            { BAKSL }
  | "&"             { ESP }
  | "&&"            { AND }
  | "||"            { OR }
  | "<"             { INF }
  | ">"             { SUP }
  | "<=?"           { LEQ }
  | ">=?"           { GEQ }
  | "<?"            { LT }
  | ">?"            { GT }
  | "=?"            { EQEQ }
  | "_"             { UNDER }
  | "->"            { ARROW }

  | eof             { EOF }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and comment depth = parse
  | "{*"            { comment (depth + 1) lexbuf }
  | "*}"            { if depth = 1 then token lexbuf
                      else comment (depth - 1) lexbuf }

  | newline         { next_line_and (comment depth) lexbuf }
  | eof             { error lexbuf "commentaire non terminé" }
  | _               { comment depth lexbuf }

