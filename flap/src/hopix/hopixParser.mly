%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF

%token IF THEN ELSE WHILE DO UNTIL FOR TO MATCH WITH FUN LET IN TYPE EXTERN REF

%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS TIMES DIV AND OR LEQ GEQ LT GT EQEQ

%token<string> TYPE_VARIABLE IDENT CONSTR

%token<int64> INT


%start<HopixAST.t> program

%%

program: EOF
{
   []
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
