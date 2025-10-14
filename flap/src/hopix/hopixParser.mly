%{ (* -*- tuareg -*- *)
  open HopixAST
  open Position
%}

%token EOF
%token IF THEN ELSE WHILE DO UNTIL FOR FROM TO MATCH WITH FUN LET IN TYPE EXTERN REF
%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS TIMES DIV AND OR LEQ GEQ LT GT EQEQ COM DOT BAR BAKSL ARROW TDOT UNDER EXL

%token<string> TYPE_VARIABLE IDENT CONSTR
%token<string> CHAR STRING
%token<int64> INT

%start<HopixAST.t> program
%%

program:
  | d=definition EOF { d }

definition:
  | 

expr:
  | x=INT         { Literal(LInt(x)) }
  | x=CHAR        { Literal(LChar(x)) }
  | x=STRING      { Literal(LString(x)) }
  | x=IDENT       { Variable(x, []) } (* TODO: Type list option *)
  | x=CONSTR      {  } (* TODO: Construction d'une donnée *)
  | LPAREN RPAREN { Tuple([]) }
  | LPAREN t=uplet_expr RPAREN { Tuple(t) }
  | LBRACE l=separated_nonempty_list(COM, label) RBRACE { Record(l, []) } (* TODO: Type list option *)
  | e=expr DOT l=IDENT { Field(e, l, []) } (* TODO: Type list option *)
  | e1=expr SEMI e2=expr { Sequence([e, e]) }
  (* | v=vdef SEMI e=expr { Define(v, e) } *)
  | BAKSL p=pattern ARROW e=expr { Fun(FunctionDefinition(p, e)) }
  | e1=expr e2=expr { Apply(e1, e2) }
  | e1=expr b=binop e2=expr { Apply(Apply(Variable(b, None), e1), e2) }
  | MATCH LPAREN e=expr RPAREN LBRACE b=branches RBRACE { Case(e, b) }
  | IF LPAREN e1=expr RPAREN THEN LBRACE e2=expr RPAREN { IfThenElse(e1, e2) } (* TODO: Else *)
  | REF e=expr    { Ref(e) }
  | e1=expr TDOT EQ e2=expr { Assign(e1, e2) }
  | EXL e=expr    { Read(e) }
  | WHILE LPAREN e1=expr RPAREN LBRACE e2=expr RBRACE { While(e1, e2) }
  | DO LBRACE e1=expr RBRACE LPAREN e2=expr RPAREN { e2, e1 }
  | FOR v=IDENT FROM e1=expr TO e2=expr LBRACE e3=expr RBRACE { For(v, e1, e2, e3) }
  | LPAREN e=expr RBRACE { e }
  (* | LPAREN e=expr TDOT t=type_ RPAREN { TypeAnnotation(e, t) } *)

uplet_expr:
  | e1=expr COM e2=expr t=uplet_rest { e1 :: e2 :: rest }

uplet_rest:
  | COM e=expr t=uplet_rest { e :: rest }
  |  { [] }

label:
  | label=IDENT EQ e=expr { (LId(label), e) }

binop:
  | PLUS {  }

branches:
  | BAR b=separated_nonempty_list(BAR, branch) { b }
  | b=separated_nonempty_list(BAR, branch) { b }

branch:
  | pattern ARROW e=expr { Branch(p, e) }

pattern:
  | v=IDENT { PVariable(v) }
  | UNDER { PWildcard }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
