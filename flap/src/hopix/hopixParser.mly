%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF

%token IF THEN ELSE WHILE DO UNTIL FOR TO MATCH WITH FUN LET IN TYPE EXTERN REF 

%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS TIMES DIV AND OR LEQ GEQ LT GT EQEQ INF SUP COM DOUBLE_POINT BAR

%token<string> TYPE_VARIABLE IDENT CONSTR

%token<int64> INT

%token<string> CHAR STRING


%start<HopixAST.t> program

%%

program:  d = definition EOF
{
  d

}

definition: 
    | TYPE i=IDENT INF t = type_variable SUP a= opt_def { DefineType(TCon i, t, a)}
    | TYPE i=IDENT a=  opt_def { DefineType(TCon i, [], a)}
    | EXTERN v=IDENT DOUBLE_POINT t = type_scheme { DeclareExtern(v, t) }
    | v=vdef         { DefineValue(v) }

opt_def:
    | EQ a=  tdefinition {a}
    | { Abstract }

type_variable : 
    | i = TYPE_VARIABLE SUP { [TId i] }
    | i1 = TYPE_VARIABLE COM i2 = type_variable { (TId i1)::i2 }



tdefinition :
    | BAR i = CONSTR l = liste_type { DefineSumType( KId i, l ) }


liste_type : 
    | t = type_ { t }
    | t1 = type_ COM t2 = liste_type { t1::t2}


type_ :  
    | i= CONSTR INF     COM 












%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
