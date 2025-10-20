%{ (* -*- tuareg -*- *)
  open HopixAST

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
%token IF THEN ELSE WHILE DO UNTIL FOR FROM TO MATCH FUN FAND LET TYPE EXTERN REF
%token LPAREN RPAREN LBRACE RBRACE SEMI EQ PLUS MINUS STAR DIV AND OR LEQ GEQ LT GT EQEQ INF SUP COM DOT BAR BAKSL ARROW DOUBLE_POINT UNDER EXL LCROCHET RCROCHET ESP

%token<string> TYPE_VARIABLE IDENT CONSTR STRING
%token<Mint.t> INT
%token<char> CHAR

%right ARROW
%right SEMI
%left EQ LT GT LEQ GEQ
%left PLUS MINUS
%left STAR DIV
%left OR AND
%right DOT DOUBLE_POINT
%nonassoc EQEQ
%left BAR ESP

%start<HopixAST.t> program
%%

program:
  | d=located(definition) p=program EOF { d::p }
  | EOF       { [] }

(**définitions*)
definition:
  | TYPE i=located(IDENT) t=opt_var a=opt_def { DefineType(loc_tcon i , t, a) }
  | EXTERN v=located(IDENT) DOUBLE_POINT t = located(type_scheme) { DeclareExtern(loc_id v ,t) }
  | v=vdefinition         { DefineValue(v) }

opt_var:
  | INF t=liste_variable SUP { t }
  |                          { [] }

type_scheme :
  | LCROCHET l=liste_variable RCROCHET t=located(type_expr) { ForallTy(l, t) }
  | t=located(type_expr) { ForallTy([], t) }

type_expr:
  | t=located(type_expr) ARROW tl=located(type_expr) { TyArrow(t, tl) }
  | t=type_uplets { t }

type_uplets:
  | t=located(type_app) STAR tl=separated_nonempty_list(STAR, located(type_app)) { TyTuple(t::tl) }
  | t=type_app { t }

type_app:
  | t=IDENT tl=type_opt         { TyCon(TCon t, tl) }
  | v=TYPE_VARIABLE             { TyVar(TId v) }
  | LPAREN t=located(type_expr) RPAREN { Position.value t }

type_opt:
  | INF t=type_list SUP { t }
  |                     { [] }

type_list:
  | t=separated_nonempty_list(COM, located(type_expr)) { t }

tdefinition :
  | bar_opt i = located(CONSTR) l = option_type_list xs = liste_constr { DefineSumType((loc_kid i, l)::xs) }
  | LBRACE i = located(IDENT) DOUBLE_POINT t=located(type_expr) l=liste_label RBRACE { DefineRecordType((loc_lid i ,t)::l) }

liste_label :
  | COM i = located(IDENT) DOUBLE_POINT t=located(type_expr) l = liste_label { ((loc_lid i),t)::l }
  | { [] }

(** Vdefinition *)
%inline vdefinition:
  | LET v=located(IDENT) s=opt_scheme EQ e=located(expr) { SimpleValue(loc_id v, s, e) }
  | FUN f=fundef l=liste_fun { RecFunctions(f::l) }

opt_scheme:
  | DOUBLE_POINT s=located(type_scheme) { Some s }
  |                                     { None }

liste_fun:
  | FAND f=fundef l=liste_fun { f::l }
  | { [] }

fundef:
  | s=opt_scheme v=located(IDENT) p=located(pattern) EQ e=located(expr) { (loc_id v, s, FunctionDefinition(p, e)) }

opt_def:
  | EQ a=tdefinition {a}
  | { Abstract }

liste_variable:
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

liste_tDef:
  | t=separated_nonempty_list(COM, located(type_expr)) { t }

(**Expressions*)
expr:
  (* for var_id from expr to expr {expr} *)
  | FOR v=located(IDENT) FROM LPAREN e1=located(expr) RPAREN TO LPAREN e2=located(expr) RPAREN LBRACE e3=located(expr) RBRACE { For(loc_id v, e1, e2, e3) }
  (* Do {expr} until (expr) ? *)
  | DO LBRACE e1=located(located(expr)) RBRACE UNTIL LPAREN e2=located(located(expr)) RPAREN { Sequence((Position.value e1)::[loc_while e2 e1]) }
  | WHILE LPAREN e1=located(expr) RPAREN LBRACE e2=located(expr) RBRACE { While(e1, e2) }
  | MATCH LPAREN e=located(expr) RPAREN LBRACE b=branches RBRACE { Case(e, b) }
  | e1=located(expr) DOUBLE_POINT EQ e2=located(expr) { Assign(e1, e2) }
  | e1=located(expr) op=located(binop) e2=located(expr){
      let loc_binop = Position.with_poss $startpos $endpos (Variable(op, None))
        in Apply (Position.with_poss $startpos $endpos (Apply (loc_binop, e1)), e2)
    }

  | ite=if_then_else                { ite }
  | BAKSL p=located(pattern) ARROW e=located(expr) { Fun(FunctionDefinition(p, e)) }
  | LBRACE l=separated_nonempty_list(COM, label) RBRACE t=type_option { Record(l, t) }
  | e=located(expr) DOT l=located(IDENT) t=type_option { Field(e, loc_lid l, t) }

  | e1=located(expr) SEMI e2=located(expr) { Sequence([e1; e2]) }
  | v=vdefinition SEMI e=located(expr) { Define(v, e) }

  | e=expr_apply                     { e }

expr_apply:
  | e1=located(expr_apply) e2=located(expr_atom) { Apply(e1, e2) }
  | e = expr_atom {e}

expr_atom:
  | x=located(IDENT) t=type_option  { Variable(loc_id x, t) }
  | x=located(STRING)               { Literal(loc_string x) }
  | x=located(CHAR)                 { Literal(loc_char x) }
  | x=located(INT)                  { Literal(loc_int x) }
  | x=located(CONSTR) t=type_option e=expr_option { Tagged(loc_kid x, t, e) }
  | LPAREN e=located(expr) DOUBLE_POINT t=located(type_expr) RPAREN { TypeAnnotation(e, t) }
  | LPAREN t=uplet_expr RPAREN      { Tuple(t) }
  | LPAREN RPAREN                   { Tuple([]) }
  | EXL e=located(expr_atom)        { Read(e) }
  | REF e=located(expr_atom)        { Ref(e) }

  | LPAREN e=expr RPAREN   { e }

%inline binop:
  | PLUS    { Id("`+`") }
  | MINUS   { Id("`-`") }
  | STAR    { Id("`*`") }
  | DIV     { Id("`/`") }
  | AND     { Id("`&&`") }
  | OR      { Id("`||`") }
  | LEQ     { Id("`<=?`") }
  | GEQ     { Id("`>=?`") }
  | LT      { Id("`<?`") }
  | GT      { Id("`>?`") }
  | EQEQ    { Id("`=?`") }

expr_option:
  | LPAREN s=separated_nonempty_list(COM, located(expr)) RPAREN { s }
  | { [] }

if_then_else:
  | IF LPAREN e1=located(expr) RPAREN THEN LBRACE e2=located(expr) RBRACE { IfThenElse(e1, e2, e2) }
  | IF LPAREN e1=located(expr) RPAREN THEN LBRACE e2=located(expr) RBRACE ELSE LBRACE e3=located(expr) RBRACE { IfThenElse(e1, e2, e3) }

type_option:
  | INF t=type_list SUP { Some t }
  | INF SUP             { Some [] }
  |                     { None }

uplet_expr:
  | e1=located(expr) COM e2=located(expr) t=uplet_rest { e1 :: e2 :: t }

uplet_rest:
  | COM e=located(expr) t=uplet_rest { e :: t }
  | { [] }

label:
  | l=located(IDENT) EQ e=located(expr) { (loc_lid l, e) }

branches:
  | BAR b=separated_nonempty_list(BAR, located(branch)) { b }
  | b=separated_nonempty_list(BAR, located(branch)) { b }

branch:
  | p=located(pattern) ARROW e=located(expr) { Branch(p, e) }

(**Pattern*)
pattern:
  | p1=located(pattern) BAR p2=located(pattern) {POr([p1; p2])}
  | p1=located(pattern) ESP p2=located(pattern) {PAnd([p1; p2])}
  | LPAREN l=separated_nonempty_list(COM, located(pattern)) RPAREN {
      match l with
      | [p] -> Position.value p
      | _   -> PTuple(l)
    }
  | p=located(pattern_simp) DOUBLE_POINT t=located(type_expr) { PTypeAnnotation(p, t) }
  | p=pattern_simp { p }

pattern_simp:
  | c = located(CONSTR) t = opt_p1 args = opt_args
      { PTaggedValue(loc_kid c, t, Option.value ~default:[] args) }
  | LBRACE fields = separated_nonempty_list(COM, record_field) RBRACE t = opt_p1 { PRecord(fields, t) }
  | a = pattern_atom { a }

record_field:
  | i = located(IDENT) EQ p = located(pattern) { (loc_lid i, p) }

pattern_atom:
  | v = located(IDENT)    { PVariable(loc_id v) }
  | UNDER                 { PWildcard }
  | x = located(INT)      { PLiteral(loc_int x) }
  | c = located(CHAR)     { PLiteral(loc_char c) }
  | s = located(STRING)   { PLiteral(loc_string s) }

opt_p1 :
  | INF t = separated_nonempty_list(COM, located(type_expr)) SUP { Some t }
  | { None }

opt_args:
  | LPAREN l = separated_nonempty_list(COM, located(pattern)) RPAREN { Some l }
  | { None }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
