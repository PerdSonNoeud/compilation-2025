%{ (* -*- tuareg -*- *)
  open HopixAST
  open Position

  let map_loc f x = Position.map f x
  
  let map_loc2 f x1 x2 = Position.join_located x1 x2 f
  let loc_list f xs = Position.join_located_list xs f

  let loc_int x = map_loc (fun s -> LInt s) x
  let loc_char x = map_loc (fun s -> LChar s) x
  let loc_string x = map_loc (fun s -> LString s) x

  let loc_id x = map_loc (fun s -> Id s) x
  let loc_tcon x = map_loc (fun s -> TCon s) x
  let loc_kid x = map_loc (fun s -> KId s) x
  let loc_tid x = map_loc (fun s -> TId s) x
  let loc_tyVar x = map_loc (fun s -> TyVar s) x
  let loc_lid x = map_loc (fun s -> LId s) x

  let loc_tyTuple x = map_loc (fun s -> TyTuple s) x 
  let loc_tuple x = map_loc (fun s -> Tuple s) x 
  let loc_read x = map_loc (fun s -> Read s) x
  let loc_ref x = map_loc (fun s -> Ref s) x
  let loc_literal x = map_loc (fun s -> Literal s) x

  let loc_for v e1 e2 e3 = map_loc (fun s -> For(v,e1,e2,e3)) v 
  let loc_tagged x t e = map_loc (fun s -> Tagged(x,t,e)) x

  let loc_tyArrow x y = map_loc2 (fun s z-> TyArrow(s,z)) x y
  let loc_assign x y = map_loc2 (fun s z-> Assign(x, y)) x y
  let loc_tyCon x y = map_loc2 (fun s z-> TyCon(s,z)) x y
  let loc_record x y = map_loc2 (fun s z-> Record(s,z)) x y 
  let loc_while x y = map_loc2 (fun s z-> While(s,z)) x y 
  let loc_case x y = map_loc2 (fun s z-> Case(s,z)) x y 
  let loc_variable x y = map_loc (fun s -> Variable (s, y)) x
  let loc_apply x y = map_loc2 (fun s z-> Apply(s,z)) x y 



%}

%token EOF
%token IF THEN ELSE WHILE DO UNTIL FOR FROM TO MATCH WITH FUN LET IN TYPE EXTERN REF
%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS STAR DIV AND OR LEQ GEQ LT GT EQEQ INF SUP COM DOT BAR BAKSL ARROW DOUBLE_POINT UNDER EXL LCROCHET RCROCHET ESP

%token<string> TYPE_VARIABLE IDENT CONSTR
%token<string> STRING
%token<int64> INT
%token<char> CHAR

%left OR
%left AND
%nonassoc EQEQ LEQ GEQ LT GT
%left PLUS MINUS
%left STAR DIV


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

    | v=vdefinition         { DefineValue(v) }

tdefinition :
    | bar_opt i = located(CONSTR) l = option_type_list xs = liste_constr { DefineSumType((loc_kid i, l)::xs) }
    | LBRACE i = located(IDENT) DOUBLE_POINT t = type_expr l = liste_label RBRACE { DefineRecordType((loc_lid i ,t)::l) }

liste_label :
    | COM i = located(IDENT) DOUBLE_POINT t = type_expr l = liste_label { ((loc_lid i),t)::l }
    | { [] }

(** Vdefinition *)
vdefinition:
    | LET v=located(IDENT) s=opt_scheme EQ e=expr { SimpleValue(loc_id v, s, e) }
    | FUN f=fundef l=liste_fun { RecFunctions(f::l) }

opt_scheme:
  | s=located(type_scheme) { Some s }
  |                        { None }

liste_fun:
    | AND f=fundef l=liste_fun { f::l }
    | { [] }

fundef:
  | s=opt_scheme v=located(IDENT) p=located(pattern) EQ e=expr { (loc_id v, s, FunctionDefinition(p, e)) }

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

type_uplets:
    | t= type_app { [t] }
    | t1 = type_uplets STAR t2= type_app { t1@[t2] }

type_app:
    | t1 = located(type_con) INF t2 = located(type_list) SUP {loc_tyCon t1 t2 }
    | t = located(type_con) { loc_tyCon t (Position.unknown_pos []) }
    | v = located(type_var) { loc_tyVar v }
    | LPAREN t = type_expr RPAREN { t }

type_list:
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
  (* | v=vdef SEMI e=expr { Define(v, e) } *)
  (* | LPAREN e=expr DOUBLE_POINT t=type_ RPAREN { TypeAnnotation(e, t) } *)
  | e=located(expr_seq)  { e }

expr_seq :
  | e = expr_assign { Position.value e }
  | e1=expr_assign e2=separated_nonempty_list(SEMI, located(expr_seq)) { Sequence(e1::e2) }

expr_assign :
  | e = located(expr_lambda) { e }
  | e1=located(expr_lambda) DOUBLE_POINT EQ e2=expr_assign { loc_assign e1 e2 }

expr_lambda :
  | e = expr_or { e }
  | BAKSL p=located(pattern) ARROW e=located(expr_or) { Fun(FunctionDefinition(p, e)) }

expr_or :
  | e = expr_and { Position.value e }
  | b = located(expr_or) OR b2 = expr_and { Apply(b,b2) }

expr_and :
  | e = located(expr_eq) { e }
  | b = located(expr_and) AND b2 = located(located(expr_eq)) { loc_apply b b2 }

expr_eq :
  | e = expr_rel { e }
  | b = located(expr_eq) EQEQ b2 = located(expr_rel) { Apply(b,b2) }

expr_rel :
  | e = expr_add { Position.value e }
  | b = located(expr_rel) LEQ b2 = expr_add { Apply(b,b2) }
  | b = located(expr_rel) GEQ b2 = expr_add { Apply(b,b2) }
  | b = located(expr_rel) LT b2 = expr_add { Apply(b,b2) }
  | b = located(expr_rel) GT b2 = expr_add { Apply(b,b2) }

expr_add :
  | e = located(expr_mul) { e }
  | b = located(expr_add) PLUS b2 = located(located(expr_mul)) { loc_apply b b2 }
  | b = located(expr_add) MINUS b2 = located(located(expr_mul)) { loc_apply b b2 }

expr_mul :
  | e = expr_app { Position.value e }
  | b = located(expr_mul) STAR b2 = expr_app { Apply(b,b2) }
  | b = located(expr_mul) DIV b2 = expr_app { Apply(b,b2) }

expr_app :
  | e = located(expr_postfix) { e }
  | e1=expr_app e2=located(expr_postfix)       { loc_assign e1 e2 }

expr_postfix :
  | e = expr_atom { Position.value e }
  | e=located(expr_postfix) DOT l=located(IDENT) t=type_option { Field(e, loc_lid l, t) } 

expr_atom:
  | x=located(INT)                  { Position.with_poss $startpos $endpos (Literal(loc_int x)) }
  | x=located(CHAR)                 { Position.with_poss $startpos $endpos (Literal(loc_char x)) }
  | x=located(STRING)               { Position.with_poss $startpos $endpos (Literal(loc_string x)) }
  | x=located(IDENT) t=type_option  { Position.with_poss $startpos $endpos (Variable((loc_id x), t)) }
  | x=located(CONSTR) t=type_option e=separated_nonempty_list(COM, expr) { loc_tagged (loc_kid x) t e }
  | LPAREN RPAREN                   { loc_tuple (Position.unknown_pos []) }
  | LPAREN e=located(expr) RPAREN            { Position.value e }
  | MATCH LPAREN e=located(expr) RPAREN LBRACE b=branches RBRACE { loc_case e b }
  | ite=located(if_then_else) { ite }
  | WHILE LPAREN e1=located(expr) RPAREN LBRACE e2=located(expr) RBRACE { loc_while e1 e2 }
  | DO LBRACE e1=located(expr) RBRACE LPAREN e2=located(expr) RPAREN { loc_while e1 e2}
  | FOR v=located(IDENT) FROM e1=expr TO e2=expr LBRACE e3=expr RBRACE { loc_for (loc_id v) e1 e2 e3 }
  | REF e=located(expr)            { loc_ref e }
  | EXL e=located(expr)            { loc_read e }
  | LPAREN t=located(uplet_expr) RPAREN { loc_tuple t}
  | LBRACE l=located(separated_nonempty_list(COM, label)) RBRACE t=located(type_option) {loc_record l t }




if_then_else:
  | IF LPAREN e1=expr RPAREN THEN LBRACE e2=expr RPAREN { IfThenElse(e1, e2, e2) }
  | IF LPAREN e1=expr RPAREN THEN LBRACE e2=expr RPAREN ELSE LBRACE e3=expr RBRACE { IfThenElse(e1, e2, e3) }

type_option:
  | t=type_list { Some t }
  |             { None }

uplet_expr:
  | e1=expr COM e2=expr t=uplet_rest { e1 :: e2 :: t }

uplet_rest:
  | COM e=expr t=uplet_rest { e :: t }
  |  { [] }

label:
  | l=located(IDENT) EQ e=expr { (loc_lid l, e) }

branches:
  | BAR b=located(separated_nonempty_list(BAR, located(branch))) { b }
  | b=located(separated_nonempty_list(BAR, located(branch))) { b }

branch:
  | p=located(pattern) ARROW e=expr {
      Branch(p, e)
    }

(**Pattern*)

pattern:
  | l = separated_nonempty_list(BAR,located(simple_pattern)) { POr(l) }
  | l = separated_nonempty_list(ESP,located(simple_pattern)) { PAnd(l) }
  | LPAREN l=separated_nonempty_list(COM,located(simple_pattern)) RPAREN { PTuple(l) }
  | p = located(pattern) DOUBLE_POINT t = type_expr { PTypeAnnotation(p,t) }
  | s = simple_pattern { s }

simple_pattern :
  | i = located(CONSTR) t = opt_p1 p=separated_list(COM, located(simple_pattern)) { PTaggedValue(loc_kid i, t, p) }
  | LBRACE i=located(IDENT) EQ p = located(simple_pattern) xs = opt_p2 RBRACE t = opt_p1 { PRecord((loc_lid i,p)::xs , t) }
  | a = atom_pattern { a }

atom_pattern :
  | v=located(IDENT) { PVariable(loc_id v) }
  | UNDER { PWildcard }
  | x = located(INT)  { PLiteral(loc_int x) }
  | c = located(CHAR) { PLiteral(loc_char c) }
  | s = located(STRING) { PLiteral(loc_string s) }

opt_p1 :
  | INF t = separated_nonempty_list(COM,type_expr) SUP { Some t }
  | { None }

opt_p2 :
  | COM i=located(IDENT) EQ p = located(simple_pattern) xs = opt_p2{ (loc_lid i,p)::xs }
  | { [] }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
