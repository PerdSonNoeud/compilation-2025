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

let digit = ['0'-'9']

let lower = ['a'-'z']

let upper = ['A'-'Z']

let identchar = ['A'-'Z' 'a'-'z' '0'-'9' '_']





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
  | "to"            { TO }
  | "match"         { MATCH }
  | "with"          { WITH }
  | "fun"           { FUN }
  | "let"           { LET }
  | "in"            { IN }
  | "type"          { TYPE }
  | "extern"        { EXTERN }
  | "ref"           { REF }


  (** Identificateurs *)
  | "'" lower identchar* as id   { TYPE_VARIABLE id }
  | lower identchar* as id       { IDENT id }
  | upper identchar* as id       { CONSTR id }

  (** Littéraux entiers *)
  | '-'? digit+ as n             { INT (Int64.of_string n) }
  | "0x" ['0'-'9' 'a'-'f' 'A' - 'F']+ as n   { INT (Int64.of_string n) }
  | "0b" ['0'-'1']+ as n         { INT (Int64.of_string n) }
  | "0o" ['0'-'7']+ as n         { INT (Int64.of_string n) }



    (** Ponctuation *)
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | ";"             { SEMI }
  | "="             { EQ }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { TIMES }
  | "/"             { DIV }
  | "&&"            { AND }
  | "||"            { OR }
  | "<=?"           { LEQ }
  | ">=?"           { GEQ }
  | "<?"            { LT }
  | ">?"            { GT }
  | "=?"           { EQEQ }

               

  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }




and comment depth = parse
  | "{*"            { comment (depth + 1) lexbuf }
  | "*}"            { if depth = 1 then token lexbuf
                      else comment (depth - 1) lexbuf }

  | newline         { next_line_and (comment depth) lexbuf }
  | eof             { error lexbuf "commentaire non terminé" }
  | _               { comment depth lexbuf }

