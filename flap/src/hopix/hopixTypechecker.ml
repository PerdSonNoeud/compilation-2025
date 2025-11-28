(** This module implements a bidirectional type checker for Hopix. *)

open HopixAST

(** Error messages *)

let invalid_instantiation pos given expected =
  HopixTypes.type_error pos (
      Printf.sprintf
        "Invalid number of types in instantiation: \
         %d given while %d were expected." given expected
    )

let check_equal_types pos ~expected ~given =
  if expected <> given
  then
    HopixTypes.(type_error pos
                  Printf.(sprintf
                            "Type mismatch.\nExpected:\n  %s\nGiven:\n  %s"
                            (string_of_aty expected)
                            (string_of_aty given)))

(** Linearity-checking code for patterns *)

let rec check_pattern_linearity
        : identifier list -> pattern Position.located -> identifier list
  = fun vars Position.{ value = p; position = pos; } ->
  let add_var vars id =
    let name = Position.value id in
    let Id(s) = name in
    if List.mem name vars then
      HopixTypes.type_error pos (
        Printf.sprintf "The variable %s has already appeared in this pattern." s
      )
    else
      name :: vars
  in
  match p with
  | PWildcard -> vars

  | PVariable id ->
    add_var vars id

  | PLiteral _ -> vars

  | PTypeAnnotation (p, _) ->
    check_pattern_linearity vars p

  | PTuple ps ->
    List.fold_left check_pattern_linearity vars ps

  | PTaggedValue (k, tyargs, ps) ->
    List.fold_left check_pattern_linearity vars ps

  | PRecord (fields, tyargs) ->
    List.fold_left (fun acc (_, p) -> check_pattern_linearity acc p) vars fields

  | PAnd ps ->
    List.fold_left (fun acc p -> check_pattern_linearity acc p) vars ps

  | POr ps ->
    let extract_names p =
      let names = check_pattern_linearity [] p in
      List.rev names
    in
    let names_list = List.map extract_names ps in
    (* all the lists should be the same *)
    let first = match names_list with [] -> [] | h::_ -> h in
    if List.exists (fun nl -> nl <> first) names_list then
      HopixTypes.type_error pos "Branches d'un or-pattern lient des variables différentes";
    List.iter (fun name ->
      if List.mem name vars then
        HopixTypes.type_error pos "Variable de pattern liée plusieurs fois (avec un OR)"
    ) first;
    List.rev_append first vars


(** Type-checking code *)

let check_type_scheme :
      HopixTypes.typing_environment ->
      Position.t ->
      HopixAST.type_scheme ->
      HopixTypes.aty_scheme * HopixTypes.typing_environment
  = fun env pos (ForallTy (ts, ty)) ->
  let ts = List.map Position.value ts in
  let new_env = HopixTypes.bind_type_variables pos env ts in
  (Scheme(ts, HopixTypes.internalize_ty new_env ty), new_env)

let normalize_op_name s =
  let len = String.length s in
  if len >= 2 && s.[0] = '`' && s.[len - 1] = '`'
  then String.sub s 1 (len - 2)
  else s

let synth_literal : HopixAST.literal -> HopixTypes.aty =
  fun l ->
    match l with
    | LInt _ -> HopixTypes.hint
    | LChar _ -> HopixTypes.hchar
    | LString _ -> HopixTypes.hstring

let instantiate_type_scheme':
      Position.t ->
      HopixTypes.aty_scheme ->
      HopixTypes.aty list ->
      HopixTypes.aty
  = fun pos scheme aty_list ->
  try
    HopixTypes.instantiate_type_scheme scheme aty_list
  with HopixTypes.InvalidInstantiation ii ->
    invalid_instantiation pos ii.given ii.expected

and lookup_type_scheme_of_constructor':
      Position.t ->
      HopixAST.constructor ->
      HopixTypes.typing_environment ->
      HopixTypes.aty_scheme
  = fun pos c env ->
  try
    HopixTypes.lookup_type_scheme_of_constructor pos c env
  with HopixTypes.Unbound (p, bind) ->
    HopixTypes.type_error p (
      Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding bind)
    )

and lookup_type_scheme_of_label':
      Position.t ->
      HopixAST.label ->
      HopixTypes.typing_environment ->
      HopixTypes.aty_scheme
  = fun pos l env ->
  try
    HopixTypes.lookup_type_scheme_of_label pos l env
  with HopixTypes.Unbound (p, bind) ->
    HopixTypes.type_error p (
      Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding bind)
    )

let rec check_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty ->
      HopixTypes.typing_environment
  = fun env Position.({ value = p; position = pos; } as pat) expected ->
  match p with
  | PWildcard ->
    env
  | PVariable id ->
    let scheme = HopixTypes.monomorphic_type_scheme expected in
    HopixTypes.bind_value (Position.value id) scheme env
  | PLiteral lit ->
    let ty = synth_literal lit.value in
    check_equal_types pos ~expected ~given:ty;
    env
  | PTypeAnnotation (p, ty) ->
    let aty = HopixTypes.internalize_ty env ty in
    check_equal_types pos ~expected ~given:aty;
    check_pattern env p aty
  | PTuple ps ->
    let expected_tys =
      match HopixTypes.destruct_product_type pos expected with
      | [] -> []
      | tys -> tys
    in
    if List.length ps <> List.length expected_tys then
      HopixTypes.type_error pos "Arity mismatch in tuple pattern";
    List.fold_left2 (fun env p ty -> check_pattern env p ty) env ps expected_tys

  | PTaggedValue (k, tyargs, ps) -> let k_ = Position.value k in
    let Scheme (params, aty) = lookup_type_scheme_of_constructor' pos k_ env in
    let aty = match tyargs with
      | None -> aty
      | Some ty ->
        let given = List.map (fun t -> HopixTypes.internalize_ty env t) ty in
        instantiate_type_scheme' pos (Scheme(params, aty)) given
    in check_equal_types pos ~expected ~given:aty;
    let arg_types, _ = HopixTypes.destruct_function_type_maximally pos aty in
    if List.length arg_types <> List.length ps then
      HopixTypes.type_error pos "Arity mismatch in tagged-value pattern";
    List.fold_left2 check_pattern env ps arg_types

  | POr ps ->
  let env0 = check_pattern env (List.hd ps) expected in
  List.iter (fun p ->
    let envi = check_pattern env p expected in
    if envi <> env0 then
      HopixTypes.type_error pos
        "Branches d'un or-pattern ne lient pas les mêmes variables"
  ) (List.tl ps);
  env0

  | PAnd ps -> List.fold_left (fun env p -> check_pattern env p expected) env ps

  | PRecord (fields, tyargs_opt) ->
      let (tc, ty_args) =
        HopixTypes.destruct_constructed_type pos expected
      in
      let expected_labels =
        HopixTypes.lookup_fields_of_type_constructor pos tc env
      in
      if List.length fields <> List.length expected_labels then
          HopixTypes.type_error pos (Printf.sprintf
            "Record pattern has wrong number of fields. Expected %d, got %d."
            (List.length expected_labels) (List.length fields));
      let final_env = List.fold_left (fun acc_env (lbl_, p) ->
          let lbl = Position.value lbl_ in
          let lbl_pos = Position.position lbl_ in
          let LId(s) = lbl in
          if not (List.mem lbl expected_labels) then 
              HopixTypes.type_error lbl_pos (Printf.sprintf "There is no type definition for label `"^s^"'.");
          let scheme = lookup_type_scheme_of_label' lbl_pos lbl env in
          let instantiated_ty = instantiate_type_scheme' lbl_pos scheme ty_args in
          let (record_ty_check, field_ty) = 
            try
              HopixTypes.destruct_function_type lbl_pos instantiated_ty
            with _ -> 
              failwith ("Internal error: type of label is not a function.")
          in
          check_equal_types lbl_pos ~expected ~given:record_ty_check;
          check_pattern acc_env p field_ty
      ) env fields in
      final_env

and synth_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty * HopixTypes.typing_environment
  = fun env (Position.{ value = p; position = pos; } as pat) ->
   let _ = ignore (check_pattern_linearity [] pat) in

  match p with
  | PWildcard | PVariable _ | PAnd _ ->
    HopixTypes.type_error pos "Unsupported pattern (synth_pattern)."

  | PLiteral lit ->
    (synth_literal lit.value, env)

  | PTypeAnnotation (p, ty) ->
    let aty = HopixTypes.internalize_ty env ty in
    let new_env = check_pattern env p aty in
    (aty, new_env)

  | PTuple ps ->
    let (tys, env') =
      List.fold_left (fun (acc_tys, acc_env) p_i ->
        let (ty_i, env_i) = synth_pattern acc_env p_i in
        (acc_tys @ [ty_i], env_i)
      ) ([], env) ps
    in
    (HopixTypes.hprod tys, env')

  | PTaggedValue (k, tyargs_opt, ps) ->
    let Scheme (params, aty) = lookup_type_scheme_of_constructor' pos k.value env in
    let tys =
      match tyargs_opt with
      | None -> []
      | Some ty -> List.map (fun t -> HopixTypes.internalize_ty env t) ty
    in
    let aty_inst = instantiate_type_scheme' pos (Scheme(params, aty)) tys in
    let arg_tys, _res = HopixTypes.destruct_function_type_maximally pos aty_inst in
    if List.length arg_tys <> List.length ps then
      HopixTypes.type_error pos "Arity mismatch in tagged-value pattern (synth_pattern)";
    let env' = List.fold_left2 (fun acc_env p_i arg_ty ->
      check_pattern acc_env p_i arg_ty
    ) env ps arg_tys in
    (_res, env')

  | PRecord (fields, tyargs_opt) ->
    let tys_in_order, env' =
      List.fold_left (fun (acc_tys, acc_env) (_lbl, p_i) ->
        let (ty_i, env_i) = synth_pattern acc_env p_i in
        (acc_tys @ [ty_i], env_i)
      ) ([], env) fields
    in
    (HopixTypes.hprod tys_in_order, env')

  | POr ps ->
    begin match ps with
    | [] -> HopixTypes.type_error pos "Or-pattern vide"
    | p0::rest ->
        let _ = check_pattern_linearity [] (Position.with_pos pos p) in
        let (ty0, env0) = synth_pattern env p0 in
        List.iter (fun p_i ->
          let (ty_i, _env_i) = synth_pattern env p_i in
          if ty_i <> ty0 then check_equal_types pos ty0 ty_i
        ) rest;
        (ty0, env0)
    end

let rec synth_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty
  = fun env Position.{ value = e; position = pos; } ->
  match e with
  | Literal l ->
    synth_literal l.value

  | Variable (id, tyargs_opt) ->
    synth_variable pos env id.value tyargs_opt

  | Apply (e1, e2) ->
    synth_apply env e1 e2

  | Tuple el ->
    let aty_list = List.map (fun e -> synth_expression env e) el in
    ATyTuple aty_list

  | Case (expr, branches) ->
    synth_case pos env expr branches

  | Define (vdef, expr) ->
    let new_env = check_value_definition env vdef in
    synth_expression new_env expr

  | Fun (FunctionDefinition (pat, expr)) ->
    let (domain_ty, param_env) = synth_pattern env pat in
    let codomain_ty = synth_expression param_env expr in
    HopixTypes.ATyArrow(domain_ty, codomain_ty)

  | Record (labels, tyargs_opt) ->
    synth_record pos env labels tyargs_opt

  | Field (e, label, ty_list) ->
    synth_field env e label ty_list

  | TypeAnnotation(e,ty) ->
    let aty = HopixTypes.internalize_ty env ty in
    check_expression env e aty;
    aty

  | Tagged(c, tyargs_opt, el) ->
    synth_tagged pos env c.value tyargs_opt el

  | Ref (expr) ->
    let aty = synth_expression env expr in
    HopixTypes.href aty

  | Assign (e1, e2) ->
    let expected = synth_expression env e1 in
    let expected = HopixTypes.destruct_reference_type e1.position expected in
    let given = synth_expression env e2 in
    (check_equal_types pos ~expected ~given);
    HopixTypes.hunit

  | Read (expr) ->
    let aty = synth_expression env expr in
    HopixTypes.destruct_reference_type expr.position aty

  | Sequence (el) ->
    synth_sequence env el

  | IfThenElse (e1, e2, e3) ->
    let given = synth_expression env e1 in
    check_equal_types e1.position ~expected:HopixTypes.hbool ~given;
    let given = synth_expression env e2 in
    check_equal_types e2.position ~expected:HopixTypes.hunit ~given;
    let given = synth_expression env e3 in
    check_equal_types e3.position ~expected:HopixTypes.hunit ~given;
    HopixTypes.hunit

  | While (e1, e2) ->
    let given = synth_expression env e1 in
    check_equal_types e1.position ~expected:HopixTypes.hbool ~given;
    synth_expression env e2

  | For (id, e1, e2, e3) ->
    let expected = HopixTypes.hint in
    let given = synth_expression env e1 in
    check_equal_types e1.position ~expected ~given;
    let given = synth_expression env e2 in
    check_equal_types e2.position ~expected ~given;
    let new_env =
      let scheme = HopixTypes.monomorphic_type_scheme expected in
      HopixTypes.bind_value id.value scheme env
    in
    check_expression new_env e3 HopixTypes.hunit;
    HopixTypes.hunit

  | _ ->
    HopixTypes.type_error pos "synth_expression: cas non implémenté"

and check_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty ->
      unit
  = fun env (Position.{ value = e; position = pos; } as exp) expected ->
  match e with
  | Fun(FunctionDefinition(p, expr)) ->
    let (t1, t2) = HopixTypes.destruct_function_type p.position expected in
    let new_env = check_pattern env p t1 in
    check_expression new_env expr t2

  | Define (vdef, expr) ->
    let new_env = check_value_definition env vdef in
    check_expression new_env expr expected

  | _ ->
    let inferred = synth_expression env exp in
    check_equal_types pos ~expected ~given:inferred

and synth_variable :
      Position.t ->
      HopixTypes.typing_environment ->
      HopixAST.identifier ->
      HopixAST.ty Position.located list option ->
      HopixTypes.aty
  = fun pos env id tyargs_opt ->
  let scheme =
    try
      HopixTypes.lookup_type_scheme_of_identifier pos id env
    with HopixTypes.Unbound(p, bind) ->
      HopixTypes.type_error p (
        Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding bind)
      )
  in
  match tyargs_opt with
  | Some ty ->
    let given =
      List.map (fun t -> HopixTypes.internalize_ty env t) ty
    in instantiate_type_scheme' pos scheme given
  | None ->
    let HopixTypes.Scheme (params, aty) = scheme in
    if List.length params = 0 then
      aty
    else HopixTypes.type_error pos "Annotation type required"

and synth_apply :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixAST.expression Position.located ->
      HopixTypes.aty
  = fun env e1 e2 ->
  let e1_aty = synth_expression env e1 in
  let (aty1, aty2) = HopixTypes.destruct_function_type e1.position e1_aty in
  check_expression env e2 aty1;
  aty2

and synth_case :
      Position.t ->
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixAST.branch Position.located list ->
      HopixTypes.aty
  = fun pos env expr branches ->
  let t_scrut = synth_expression env expr in
  match branches with
    | [] -> HopixTypes.type_error pos "Match sans branches"
    | (Position.{ value = Branch (pat, expr); position = bpos }) :: rest ->
      (* première branche *)
      let (pat_ty0, env_pat0) = synth_pattern env pat in
      check_equal_types (Position.position pat) ~expected:t_scrut ~given:pat_ty0;
      let ty_branch0 = synth_expression env_pat0 expr in
      (* autres branches  *)
      List.iter (fun (Position.{ value = Branch (pat_i, expr_i); position = bpos_i }) ->
        let (pat_tyi, env_pati) = synth_pattern env pat_i in
        check_equal_types (Position.position pat_i) ~expected:t_scrut ~given:pat_tyi;
        let ty_br_i = synth_expression env_pati expr_i in
        if ty_br_i <> ty_branch0 then
          HopixTypes.type_error bpos_i "Types de branches incompatibles dans le match"
      ) rest;
      ty_branch0

and synth_record :
      Position.t ->
      HopixTypes.typing_environment ->
      (HopixAST.label Position.located * HopixAST.expression Position.located) list ->
      HopixAST.ty Position.located list option ->
      HopixTypes.aty
  = fun pos env labels tyargs_opt ->
  let (l, e) = List.hd labels in
  match tyargs_opt with
  | Some tyargs ->
    let (tc, _, ll) =
      try
        HopixTypes.lookup_type_constructor_of_label l.position l.value env
      with HopixTypes.Unbound (p, binding) ->
        HopixTypes.type_error p
        (Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding binding))
    in

    let aty_list = List.map (fun ty -> HopixTypes.internalize_ty env ty) tyargs in
    List.iter (fun (l, _) ->
      if (not (List.mem (Position.value l) ll)) then
      HopixTypes.type_error l.position
      "Ill-formed type: one or multiple elements of the record are missing."
    ) labels;
    check_record_expressions pos env labels aty_list;
    ATyCon (tc, aty_list)

  | None ->
    HopixTypes.type_error e.position
    "Ill-formed type: anotation of type required for a variable."

and check_record_expressions :
      Position.t ->
      HopixTypes.typing_environment ->
      (HopixAST.label Position.located * HopixAST.expression Position.located) list ->
      HopixTypes.aty list ->
      unit
  = fun pos env labels aty_list ->
  List.iter (fun (Position.{position = l_pos; value = l}, e) ->
    let scheme =
      lookup_type_scheme_of_label' l_pos l env
    in
    let aty = instantiate_type_scheme' pos scheme aty_list in
    let (_, aty) = HopixTypes.destruct_function_type l_pos aty in
    check_expression env e aty
  ) labels

and synth_field :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixAST.label Position.located ->
      HopixAST.ty Position.located list option ->
      HopixTypes.aty
  = fun env e (Position.{value = l; position = pos} as label) ty_list ->
  (* Get the type of the expression *)
  let aty = synth_expression env e in
  match aty with
  | ATyCon (tc, aty_list) ->
    let labels =
      HopixTypes.lookup_fields_of_type_constructor e.position tc env
    in
    let l_ = HopixTypes.Label l in
    (* Verifying that the label exists *)
    if (not (List.mem l labels)) then
      HopixTypes.type_error pos
      (Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding l_));
    verify_field_types env e label aty_list ty_list

  | _ ->
    HopixTypes.type_error e.position "Ill-formed type: the expression should be a constructor."

and verify_field_types :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixAST.label Position.located ->
      HopixTypes.aty list ->
      HopixAST.ty Position.located list option ->
      HopixTypes.aty
  = fun env e (Position.{value = l; position = pos} as label) expected tyargs_opt ->
  let scheme = lookup_type_scheme_of_label' pos l env in
  let Scheme (_, aty) = scheme in
  match tyargs_opt with
  | Some tyargs ->
    (* Given aty list *)
    let given = List.map (fun ty -> HopixTypes.internalize_ty env ty) tyargs in
    (* Compare the provided type list to the type constructor type list *)
    List.iter2 (fun expected given ->
      check_equal_types (Position.position e) ~expected ~given
    ) expected given;
    let aty = instantiate_type_scheme' pos scheme given in
    let (_, aty) = HopixTypes.destruct_function_type pos aty in
    aty
  | None ->
    let (_, aty) = HopixTypes.destruct_function_type pos aty in
    aty

and synth_tagged :
      Position.t ->
      HopixTypes.typing_environment ->
      HopixAST.constructor ->
      HopixAST.ty Position.located list option ->
      HopixAST.expression Position.located list ->
      HopixTypes.aty
  = fun pos env c tyargs_opt el ->
  (* Alternative function to HopixTypes.destruct_function_type_maximally
     that does it for n elements instead of all of them and use
     HopixTypes.destruct_function_type to verify if it is the right type. *)
  let rec destruct_function_type_nth n = function
    | HopixTypes.ATyArrow (ins, out) when n > 0 ->
      let (_, _) = HopixTypes.destruct_function_type pos out in
      let ins', out = destruct_function_type_nth (n - 1) out in
      (ins :: ins', out)
    | ty ->
      ([], ty)
  in
  (* Scheme of constructor *)
  let Scheme (params, aty) as s =
    lookup_type_scheme_of_constructor' pos c env
  in
  (* Expression list to aty list *)
  let aty_list = List.map (fun e -> synth_expression env e) el in

  let aty_inst =
    match tyargs_opt with
    | None -> failwith "none"
    | Some ty ->
      let given = List.map (fun t -> HopixTypes.internalize_ty env t) ty in
      instantiate_type_scheme' pos s given
  in
  let arg_tys, res = HopixTypes.destruct_function_type_maximally pos aty_inst in

  try
    (* Same size *)
    let env' = List.iter2 (fun e arg_ty ->
      check_expression env e arg_ty
    ) el arg_tys; in
    res
  with _ ->
    (* Different size *)
    let smallest_list = min (List.length el) (List.length arg_tys) in
    let (_, result) = destruct_function_type_nth smallest_list aty_inst in
    let e = (List.nth el (smallest_list - 1)) in
    let aty = synth_expression env e in
    check_equal_types (Position.position e) ~expected:aty ~given:result;
    aty

and synth_sequence :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located list ->
      HopixTypes.aty
  = fun env el ->
  match el with
  (* Empty sequence, should not happen but just in case *)
  | [] ->
    HopixTypes.hunit
  (* Last expression of the sequence, we return its type *)
  | [e] ->
    synth_expression env e
  (* In a sequence, each expression must return unit type, except the last one *)
  | h::t ->
    check_expression env h HopixTypes.hunit;
    synth_sequence env t

and check_value_definition :
      HopixTypes.typing_environment ->
      HopixAST.value_definition ->
      HopixTypes.typing_environment
  = fun env def ->
  match def with
  | SimpleValue (id, ts, expr) ->
    begin match ts with
    | None    -> failwith "Missing type annotation"
    | Some ts ->
      let (s, new_env) = check_type_scheme env ts.position ts.value in
      let Scheme (vl, aty) = s in
      check_expression new_env expr aty;
      HopixTypes.bind_value id.value s new_env
    end

  | RecFunctions recfun ->
    check_rec_fun env recfun

and check_rec_fun :
      HopixTypes.typing_environment ->
      HopixAST.function_definition HopixAST.polymorphic_definition list ->
      HopixTypes.typing_environment
  = fun env recfun ->

  let fundata =
    List.map (fun (id, ts, fundef) ->
      match ts with
      | None -> failwith "Type annotation is missing for recursive function"
      | Some (Position.{ value = e; position = pos }) ->
          let (s, new_env) = check_type_scheme env pos e in
          (id, s, fundef, new_env)
    ) recfun
  in

  (* create env *)
  let fun_env =
    List.fold_left (fun env_acc (id, s, _, _) ->
      HopixTypes.bind_value (Position.value id) s env_acc
    ) env fundata
  in

  (* verif function body *)
  List.iter (fun (id, s, fundef, new_env) ->
    let check_env =
      List.fold_left (fun acc (id2, sch2, _, _) ->
        HopixTypes.bind_value (Position.value id2) sch2 acc
      ) new_env fundata
    in
    let HopixTypes.Scheme(_, aty) = s in
    let (param_ty, return_ty) =
      HopixTypes.destruct_function_type (Position.position id) aty
    in
    let FunctionDefinition(pat, expr) = fundef in
    let param_env = check_pattern check_env pat param_ty in
    check_expression param_env expr return_ty
  ) fundata;

  fun_env


let check_definition env = function
  | DefineValue vdef ->
     check_value_definition env vdef

  | DefineType (t, ts, tdef) ->
     let ts = List.map Position.value ts in
     HopixTypes.bind_type_definition (Position.value t) ts tdef env

  | DeclareExtern (x, tys) ->
     let tys, _ = Position.located_pos (check_type_scheme env) tys in
     HopixTypes.bind_value (Position.value x) tys env

let typecheck env program =
  List.fold_left
    (fun env d -> Position.located (check_definition env) d)
    env program

type typing_environment = HopixTypes.typing_environment

let initial_typing_environment = HopixTypes.initial_typing_environment

let print_typing_environment = HopixTypes.string_of_typing_environment
