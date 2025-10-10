%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position
  
  let map_loc f x = Position.map f x

  
  let map_loc2 f x1 x2 = Position.join_located x1 x2 f
  let loc_list f xs = Position.join_located_list xs f

  let loc_id x = map_loc (fun s -> Id s) x
  let loc_tcon x = map_loc (fun s -> TCon s) x
  let loc_kid x = map_loc (fun s -> KId s) x
  let loc_tid x = map_loc (fun s -> TId s) x
  let loc_tyVar x = map_loc (fun s -> TyVar s) x

  let loc_tyTuple x = map_loc (fun s -> TyTuple s) x 

  let loc_tyArrow x y = map_loc2 (fun s z-> TyArrow(s,z)) x y
  let loc_tyCon x y = map_loc2 (fun s z-> TyCon(s,z)) x y


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
    | d = located(definition) p = program EOF { d::p }
    | EOF       { [] }

(**définitions*)

definition: 
    | TYPE i=located(IDENT) INF t = liste_variable SUP a= opt_def { 
        (DefineType(loc_tcon i , t, a))}

    | TYPE i=located(IDENT) a=  opt_def { 
        (DefineType(loc_tcon i , [], a))}

    | EXTERN v=located(IDENT) DOUBLE_POINT t = located(type_scheme) { 
        (DeclareExtern(loc_id v ,t)) }

    (*| v=vdef         { DefineValue(v) }*)

opt_def:
    | EQ a=  tdefinition {a}
    | { Abstract }


liste_variable : 
    | i1 = located(TYPE_VARIABLE) COM i2 = liste_variable {loc_tid i1::i2 }
    | i = located(TYPE_VARIABLE) { [loc_tid i] }




tdefinition :
    | BAR i = located(CONSTR) l = option_type_list { DefineSumType[ (loc_kid i, l) ] }


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
                | l ->  Position.with_poss $startpos $endpos (TyTuple l)
            in Position.with_poss $startpos $endpos (TyArrow (aux, t2))
        }
    | t = located(type_uplets) {loc_tyTuple t }

type_uplets :
    | t= type_app { [t] }
    | t1 = type_uplets STAR t2= type_app { t1@[t2] }

type_app : 
    | t1 = located(type_con) INF t2 = located(type_list) SUP {loc_tyCon t1 t2 }
    | t = located(type_con) { loc_tyCon t (Position.unknown_pos []) }
    | v = located(type_var) { loc_tyVar v }
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
