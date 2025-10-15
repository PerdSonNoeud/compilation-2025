%{ (* -*- tuareg -*- *)
  open HopixAST
  open Position
<<<<<<< HEAD
%}

%token EOF
%token IF THEN ELSE WHILE DO UNTIL FOR FROM TO MATCH WITH FUN LET IN TYPE EXTERN REF
%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS TIMES DIV AND OR LEQ GEQ LT GT EQEQ COM DOT BAR BAKSL ARROW TDOT UNDER EXL
=======
  
  let map_loc f x = Position.map f x

  
  let map_loc2 f x1 x2 = Position.join_located x1 x2 f
  let loc_list f xs = Position.join_located_list xs f

  let loc_id x = map_loc (fun s -> Id s) x
  let loc_tcon x = map_loc (fun s -> TCon s) x
  let loc_kid x = map_loc (fun s -> KId s) x
  let loc_tid x = map_loc (fun s -> TId s) x
  let loc_tyVar x = map_loc (fun s -> TyVar s) x
  let loc_lid x = map_loc (fun s -> LId s) x

  let loc_tyTuple x = map_loc (fun s -> TyTuple s) x 

  let loc_tyArrow x y = map_loc2 (fun s z-> TyArrow(s,z)) x y
  let loc_tyCon x y = map_loc2 (fun s z-> TyCon(s,z)) x y


%}

%token EOF

%token IF THEN ELSE WHILE DO UNTIL FOR TO MATCH WITH FUN LET IN TYPE EXTERN REF 

%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS STAR DIV AND OR LEQ GEQ LT GT EQEQ INF SUP COM DOUBLE_POINT ARROW BAR LCROCHET RCROCHET
>>>>>>> Jalon_1

%token<string> TYPE_VARIABLE IDENT CONSTR
%token<string> CHAR STRING
%token<int64> INT

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

    (*| v=vdefinition         { DefineValue(v) }*)

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

tdefinition :
    | bar_opt i = located(CONSTR) l = option_type_list xs = liste_constr { DefineSumType((loc_kid i, l)::xs) }
    | LBRACE i = located(IDENT) DOUBLE_POINT t = type_expr l = liste_label RBRACE { DefineRecordType((loc_lid i ,t)::l) }

liste_label :
    | COM i = located(IDENT) DOUBLE_POINT t = type_expr l = liste_label { ((loc_lid i),t)::l }
    | { [] }


(*
vdefinition :
    | LET v=located(IDENT) opt_sheme EQ e = expr {  }
    | FUN f=fundef l = liste_fun {  }
*)

opt_def:
    | EQ a=  tdefinition {a}
    | { Abstract }


liste_variable : 
    | i1 = located(TYPE_VARIABLE) COM i2 = liste_variable {loc_tid i1::i2 }
    | i = located(TYPE_VARIABLE) { [loc_tid i] }


bar_opt :
    | BAR {}
    | {}

liste_constr :
    | BAR i = located(CONSTR) l = option_type_list xs = liste_constr { (loc_kid i, l)::xs }
    | { [] }


option_type_list:
    | LPAREN l = liste_tDef RPAREN { l }
    | { [] }

liste_tDef : 
    | t = type_expr { [t] }
    | t1 = type_expr COM t2 = liste_tDef { t1::t2}

(*
liste_fun :
    | AND f = fundef l = liste_fun {  }
    | {  }
*)

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

type_scheme : 
    | LCROCHET l = liste_variable RCROCHET t= type_expr { ForallTy(l,t) }


%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
