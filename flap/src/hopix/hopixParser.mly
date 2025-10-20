%{ (* -*- tuareg -*- *)
  open HopixAST

  let map_loc f x = Position.map f x

  let map_loc2 f x1 x2 = Position.join_located x1 x2 f

  let loc_int x = map_loc (fun s -> LInt s) x
  let loc_char x = map_loc (fun s -> LChar s) x
  let loc_string x = map_loc (fun s -> LString s) x

  let loc_id x = map_loc (fun s -> Id s) x
  let loc_tcon x = map_loc (fun s -> TCon s) x
  let loc_kid x = map_loc (fun s -> KId s) x
  let loc_tid x = map_loc (fun s -> TId s) x
  let loc_lid x = map_loc (fun s -> LId s) x

  let loc_while x y = map_loc2 (fun s z-> While(s,z)) x y
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
  | EXTERN v=located(IDENT) DOUBLE_POINT t=located(type_scheme) { DeclareExtern(loc_id v ,t) }
  | v=vdefinition         { DefineValue(v) }

opt_var:
  | INF t=liste_variable SUP { t }
  |                          { [] }

liste_variable:
  | i1=located(TYPE_VARIABLE) COM i2=liste_variable {loc_tid i1::i2 }
  | i=located(TYPE_VARIABLE) { [loc_tid i] }

opt_def:
  | EQ a=tdefinition { a }
  | { Abstract }

type_scheme :
  | LCROCHET l=liste_variable RCROCHET t=located(type_expr) { ForallTy(l, t) }
  | t=located(type_expr) { ForallTy([], t) }

(** TDefinition *)
tdefinition :
  | BAR i=located(CONSTR) l=option_type_list xs=liste_constr { DefineSumType((loc_kid i, l)::xs) }
  | i=located(CONSTR) l=option_type_list xs=liste_constr { DefineSumType((loc_kid i, l)::xs) }
  | LBRACE i=located(IDENT) DOUBLE_POINT t=located(type_expr) l=liste_label RBRACE { DefineRecordType((loc_lid i ,t)::l) }

liste_constr :
  | BAR i = located(CONSTR) l = option_type_list xs = liste_constr { (loc_kid i, l)::xs }
  | { [] }

option_type_list:
  | LPAREN l=separated_nonempty_list(COM, located(type_expr)) RPAREN { l }
  | { [] }

liste_label :
  | COM i= located(IDENT) DOUBLE_POINT t=located(type_expr) l = liste_label { ((loc_lid i),t)::l }
  | { [] }

type_expr:
  | t=located(type_expr) ARROW tl=located(type_expr) { TyArrow(t, tl) }
  | t=located(type_app) STAR tl=separated_nonempty_list(STAR, located(type_app)) { TyTuple(t::tl) }
  | t=type_app { t }

type_app:
  | t=IDENT tl=type_opt       { TyCon(TCon t, tl) }
  | v=TYPE_VARIABLE           { TyVar(TId v) }
  | LPAREN t=type_expr RPAREN { t }

type_opt:
  | INF t=type_list SUP { t }
  |                     { [] }

type_list:
  | t=separated_nonempty_list(COM, located(type_expr)) { t }

(** Vdefinition *)
%inline vdefinition:
  | LET v=located(IDENT) s=opt_scheme EQ e=located(expr) { SimpleValue(loc_id v, s, e) }
  | FUN f=fundef l=liste_fun { RecFunctions(f::l) }

opt_scheme:
  | DOUBLE_POINT s=located(type_scheme) { Some s }
  |                                     { None }

fundef:
  | s=opt_scheme v=located(IDENT) p=located(pattern) EQ e=located(expr) { (loc_id v, s, FunctionDefinition(p, e)) }

liste_fun:
  | FAND f=fundef l=liste_fun { f::l }
  | { [] }

(** Expressions *)
expr:
  (* Boucle bornée *)
  | FOR v=located(IDENT) FROM LPAREN e1=located(expr) RPAREN TO LPAREN e2=located(expr) RPAREN LBRACE e3=located(expr) RBRACE
    { For(loc_id v, e1, e2, e3) }
  (* Boucle non bornée et non vide *)
  | DO LBRACE e1=located(located(expr)) RBRACE UNTIL LPAREN e2=located(located(expr)) RPAREN
    { Sequence((Position.value e1)::[loc_while e2 e1]) }
  (* Boucle non bornée *)
  | WHILE LPAREN e1=located(expr) RPAREN LBRACE e2=located(expr) RBRACE
    { While(e1, e2) }
  (* Analyse de motifs *)
  | MATCH LPAREN e=located(expr) RPAREN LBRACE b=branches RBRACE
    { Case(e, b) }
  (* Affectation *)
  | e1=located(expr) DOUBLE_POINT EQ e2=located(expr) { Assign(e1, e2) }
  (* Conditionnelle *)
  | ite=if_then_else { ite }
  (* Fonction anonyme *)
  | BAKSL p=located(pattern) ARROW e=located(expr) { Fun(FunctionDefinition(p, e)) }
  (* Construction d'un enregistrement *)
  | LBRACE l=separated_nonempty_list(COM, label) RBRACE t=type_option { Record(l, t) }
  (* Projection d'un champ *)
  | e=located(expr) DOT l=located(IDENT) t=type_option { Field(e, loc_lid l, t) }
  (* Séquencement *)
  | e1=located(expr) SEMI e2=located(expr) { Sequence([e1; e2]) }
  (* Définition locale *)
  | v=vdefinition SEMI e=located(expr) { Define(v, e) }
  (* Application infixe *)
  | e1=located(expr) op=located(binop) e2=located(expr){
      let loc_binop = Position.with_poss $startpos $endpos (Variable(op, None))
        in Apply (Position.with_poss $startpos $endpos (Apply (loc_binop, e1)), e2)
    }
  | e=expr_apply { e }

expr_apply:
  (* Application *)
  | e1=located(expr_apply) e2=located(expr_atom) { Apply(e1, e2) }
  | e = expr_atom {e}

expr_atom:
  (* Variable *)
  | x=located(IDENT) t=type_option { Variable(loc_id x, t) }
  (* Chaine de caractère *)
  | x=located(STRING)              { Literal(loc_string x) }
  (* Caractère *)
  | x=located(CHAR)                { Literal(loc_char x) }
  (* Entier positif *)
  | x=located(INT)                 { Literal(loc_int x) }
  (* Construction d'une donnée *)
  (* TODO: Fix le conflit shift/reduce *)
  | x=located(CONSTR) t=type_option e=expr_option { Tagged(loc_kid x, t, e) }
  (* Annotation de type *)
  | LPAREN e=located(expr) DOUBLE_POINT t=located(type_expr) RPAREN { TypeAnnotation(e, t) }
  (* Construction d'un n-uplets (n > 1) *)
  | LPAREN t=uplet_expr RPAREN     { Tuple(t) }
  (* Construction d'un 0-uplets *)
  | LPAREN RPAREN                  { Tuple([]) }
  (* Lecture *)
  | EXL e=located(expr_atom)       { Read(e) }
  (* Allocation *)
  | REF e=located(expr_atom)       { Ref(e) }
  | LPAREN e=expr RPAREN           { e }

(** Branches *)
branches:
  | BAR b=separated_nonempty_list(BAR, located(branch)) { b }
  | b=separated_nonempty_list(BAR, located(branch)) { b }

branch:
  | p=located(pattern) ARROW e=located(expr) { Branch(p, e) }

(** Pattern *)
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
  | c=located(CONSTR) t=opt_p1 args=opt_args { PTaggedValue(loc_kid c, t, Option.value ~default:[] args) }
  | LBRACE fields=separated_nonempty_list(COM, record_field) RBRACE t=opt_p1 { PRecord(fields, t) }
  | v=located(IDENT)    { PVariable(loc_id v) }
  | UNDER                 { PWildcard }
  | x=located(INT)      { PLiteral(loc_int x) }
  | c=located(CHAR)     { PLiteral(loc_char c) }
  | s=located(STRING)   { PLiteral(loc_string s) }

record_field:
  | i=located(IDENT) EQ p=located(pattern) { (loc_lid i, p) }

opt_p1 :
  | INF t=separated_nonempty_list(COM, located(type_expr)) SUP { Some t }
  | { None }

opt_args:
  | LPAREN l=separated_nonempty_list(COM, located(pattern)) RPAREN { Some l }
  | { None }

if_then_else:
  | IF LPAREN e1=located(expr) RPAREN THEN LBRACE e2=located(expr) RBRACE { IfThenElse(e1, e2, e2) }
  | IF LPAREN e1=located(expr) RPAREN THEN LBRACE e2=located(expr) RBRACE ELSE LBRACE e3=located(expr) RBRACE { IfThenElse(e1, e2, e3) }

label:
  | l=located(IDENT) EQ e=located(expr) { (loc_lid l, e) }

type_option:
  | INF t=type_list SUP { Some t }
  | INF SUP             { Some [] }
  |                     { None }

%inline binop:
  | PLUS  { Id("`+`") }
  | MINUS { Id("`-`") }
  | STAR  { Id("`*`") }
  | DIV   { Id("`/`") }
  | AND   { Id("`&&`") }
  | OR    { Id("`||`") }
  | LEQ   { Id("`<=?`") }
  | GEQ   { Id("`>=?`") }
  | LT    { Id("`<?`") }
  | GT    { Id("`>?`") }
  | EQEQ  { Id("`=?`") }

expr_option:
  | LPAREN s=separated_nonempty_list(COM, located(expr)) RPAREN { s }
  | { [] }

uplet_expr:
  | e1=located(expr) COM e2=located(expr) t=uplet_rest { e1 :: e2 :: t }

uplet_rest:
  | COM e=located(expr) t=uplet_rest { e :: t }
  | { [] }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
