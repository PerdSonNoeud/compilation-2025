%{ (* -*- tuareg -*- *)
  open HopixAST
  open Position

  let map_loc f x = Position.map f x
  
  let map_loc2 f x1 x2 = Position.join_located x1 x2 f
  let loc_list f xs = Position.join_located_list xs f

  let loc_int x = map_loc (fun s -> LInt s) x
  let loc_string x = map_loc (fun s -> LString s) x
  let loc_char x = map_loc (fun s -> LChar s) x

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
%token IF THEN ELSE WHILE DO UNTIL FOR FROM TO MATCH WITH FUN LET IN TYPE EXTERN REF
%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS STAR DIV AND OR LEQ GEQ LT GT EQEQ INF SUP COM DOT BAR BAKSL ARROW DOUBLE_POINT UNDER EXL LCROCHET RCROCHET ESP

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
    | EQ a=tdefinition {a}
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


type_scheme :
    | LCROCHET l = liste_variable RCROCHET t= type_expr { ForallTy(l,t) }

(**Expressions*)

expr:
  (* Loops *)
  | WHILE LPAREN e1=expr RPAREN LBRACE e2=expr RBRACE { While(e1, e2) }
  | DO LBRACE e1=expr RBRACE LPAREN e2=expr RPAREN { e2, e1 }
  | FOR v=located(IDENT) FROM e1=expr TO e2=expr LBRACE e3=expr RBRACE { For(loc_id v, e1, e2, e3) }
  (* Functions *)
  | BAKSL p=pattern ARROW e=expr { Fun(FunctionDefinition(p, e)) }
  (* Binary operations *)
  | e1=expr b=binop e2=expr { Apply(Apply(Variable(b, None), e1), e2) }
  (* Others *)
  | MATCH LPAREN e=expr RPAREN LBRACE b=branches RBRACE { Case(e, b) }
  | IF LPAREN e1=expr RPAREN THEN LBRACE e2=expr RPAREN { IfThenElse(e1, e2) } (* TODO: Else *)
  | LPAREN t=uplet_expr RPAREN { Tuple(t) }
  | LBRACE l=separated_nonempty_list(COM, label) RBRACE { Record(l, None) } (* TODO: Type list option *)
  | e=expr DOT l=located(IDENT) { Field(e, loc_id l, None) } (* TODO: Type list option *)
  | e1=expr SEMI e2=expr { Sequence([e, e]) }
  (* | v=vdef SEMI e=expr { Define(v, e) } *)
  | e1=expr DOUBLE_POINT EQ e2=expr { Assign(e1, e2) }
  (* | LPAREN e=expr DOUBLE_POINT t=type_ RPAREN { TypeAnnotation(e, t) } *)
  | e=expr_simp           { e }

expr_simp:
  | e1=expr_atom e2=expr  { Apply(e1, e2) }
  | REF e=expr            { Ref(e) }
  | EXL e=expr            { Read(e) }
  | e=expr_atom           { e }

expr_atom:
  | x=located(INT)        { Literal(loc_int x) }
  | x=located(CHAR)       { Literal(loc_char x) }
  | x=located(STRING)     { Literal(loc_string x) }
  | x=located(IDENT)      { Variable(loc_id x, None) } (* TODO: Type list option *)
  | x=located(CONSTR)     { Tagged(loc_kid x, None, None) } (* TODO: Type list option & Expr list option *)
  | LPAREN RPAREN         { Tuple([]) }
  | LPAREN e=expr RBRACE  { e }

uplet_expr:
  | e1=expr COM e2=expr t=uplet_rest { e1 :: e2 :: t }

uplet_rest:
  | COM e=expr t=uplet_rest { e :: t }
  |  { [] }

label:
  | l=located(IDENT) EQ e=expr { (loc_id l, e) }

binop:
  | PLUS {  }

branches:
  | BAR b=separated_nonempty_list(BAR, branch) { b }
  | b=separated_nonempty_list(BAR, branch) { b }

branch:
  | pattern ARROW e=expr { Branch(p, e) }


(**Pattern*)

pattern:
  | LPAREN l=separated_nonempty_list(COM,simple_pattern) RPAREN { PTuple(l) }
  | p = pattern DOUBLE_POINT t = type_expr { PTypeAnnotation(p,t) }
  | l = separated_nonempty_list(BAR,simple_pattern) { POr(l) }
  | l = separated_nonempty_list(ESP,simple_pattern) { PAnd(l) }
  | s = simple_pattern { s }

simple_pattern :
  | i = located(CONSTR) t = opt_p1 p = separated_list(COM,simple_pattern) { PTaggedValue(loc_kid i, t, p) }
  | LBRACE i=located(IDENT) EQ p = simple_pattern xs = opt_p2 RBRACE t = opt_p1 { PRecord((loc_kid i,p)::xs , t) }


atom_pattern :
  | v=located(IDENT) { PVariable(loc_id v) }
  | UNDER { PWildcard }
  | x = INT  { Literal(loc_lint x) }
  | c = CHAR { Literal(loc_lchar c) }
  | s = STRING { Literal(loc_lstring s) }


opt_p1 :
  | MINUS t = separated_nonempty_list(COM,type_expr) PLUS { SOME t }
  | { None }

opt_p2 :
  | COM i=located(IDENT) EQ p = simple_pattern xs = opt_p2{ (loc_kid i,p)::p2 }
  | { }






%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
