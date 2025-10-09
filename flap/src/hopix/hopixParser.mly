%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF

%token IF THEN ELSE WHILE DO UNTIL FOR TO MATCH WITH FUN LET IN TYPE EXTERN REF 

%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS STAR DIV AND OR LEQ GEQ LT GT EQEQ INF SUP COM DOUBLE_POINT ARROW BAR LCROCHET RCROCHET

%token<string> TYPE_VARIABLE IDENT CONSTR

%token<int64> INT

%token<string> CHAR STRING


%start<HopixAST.t> program

%%

program:  
    | d = definition p = program EOF { d::p }
    | EOF       { [] }

(**définitions*)

definition: 
    | TYPE i=IDENT INF t = liste_variable SUP a= opt_def { 
        Position.with_poss $startpos $endpos (DefineType(Position.with_poss $startpos $endpos (TCon i), t, a))}
    | TYPE i=IDENT a=  opt_def { 
        Position.with_poss $startpos $endpos (DefineType(Position.with_poss $startpos $endpos (TCon i), [], a))}
    | EXTERN v=IDENT DOUBLE_POINT t = type_scheme { 
        Position.with_poss $startpos $endpos (DeclareExtern(Position.with_poss $startpos $endpos (Id v),Position.with_poss $startpos $endpos t)) }
    (*| v=vdef         { DefineValue(v) }*)

opt_def:
    | EQ a=  tdefinition {a}
    | { Abstract }

liste_variable : 
    | i1 = TYPE_VARIABLE COM i2 = liste_variable { (Position.with_poss $startpos $endpos (TId i1))::i2 }
    | i = TYPE_VARIABLE { [Position.with_poss $startpos $endpos (TId i)] }



tdefinition :
    | BAR i = CONSTR l = option_type_list { DefineSumType[ (Position.with_poss $startpos $endpos (KId i), l) ] }


option_type_list:
    | LPAREN l = liste_tDef RPAREN { l }
    | { [] }

liste_tDef : 
    | t = type_expr { [t] }
    | t1 = type_expr COM t2 = liste_tDef { t1::t2}



(**syntaxe des types*)

type_expr :  
    | t1= type_arrow { t1 } 


type_arrow :
    | t1 = type_uplets ARROW t2 = type_arrow {
        let aux =
            match t1 with
                | [x] -> x
                | l -> Position.with_poss $startpos $endpos (TyTuple l)
            in Position.with_poss $startpos $endpos (TyArrow (aux, t2))
        }
    | t = type_uplets { Position.with_poss $startpos $endpos (TyTuple t) }

type_uplets :
    | t= type_app { [t] }
    | t1 = type_uplets STAR t2= type_app { t1@[t2] }

type_app : 
    | t1 = type_con INF t2 = type_list SUP { Position.with_poss $startpos $endpos (TyCon(t1,t2)) }
    | t = type_con { Position.with_poss $startpos $endpos (TyCon (t, [])) }
    | v = type_var { Position.with_poss $startpos $endpos (TyVar v) }
    | LPAREN t = type_expr RPAREN { t }

type_list : 
    | t= type_expr { [t] }
    | t1= type_list COM t2= type_expr { t1@[t2] }

type_con:
    | i = IDENT { TCon i }

type_var:
    | i = TYPE_VARIABLE { TId i }



type_scheme : 
    | LCROCHET l = liste_variable RCROCHET t= type_expr { ForallTy(l,t) }





%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
