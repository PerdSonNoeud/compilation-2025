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
    if List.mem name vars then
      HopixTypes.type_error pos "Variable de pattern liée plusieurs fois"
    else
      name :: vars
  in
  match p with
  | PWildcard -> vars

  | PVariable id ->
    add_var vars id

  | PLiteral _ -> vars

  | PTypeAnnotation (p, ty) ->
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


let rec check_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty ->
      HopixTypes.typing_environment
  = fun env Position.({ value = p; position = pos; } as pat) expected ->
  match p with
    | PWildcard -> env

    | PVariable id -> HopixTypes.bind_value (Position.value id) (Scheme([], expected)) env

    | PLiteral lit -> let ty = synth_literal lit.value in check_equal_types pos ~expected ~given:ty; env

    | PTypeAnnotation (p, ty) -> let ty_ = HopixTypes.internalize_ty env ty in 
      check_equal_types pos ~expected ~given:ty_; check_pattern env p ty_

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
      let Scheme (params, aty) = HopixTypes.lookup_type_scheme_of_constructor pos k_ env in
      let aty = match tyargs with
        | None -> aty
        | Some ty ->
          let given = List.map (fun t -> HopixTypes.internalize_ty env t) ty in
          HopixTypes.instantiate_type_scheme (Scheme(params, aty)) given
      in check_equal_types pos ~expected ~given:aty;
      let arg_types, _ = HopixTypes.destruct_function_type_maximally pos aty in
      if List.length arg_types <> List.length ps then
        HopixTypes.type_error pos "Arity mismatch in tagged-value pattern";
      List.fold_left2 check_pattern env ps arg_types

    | POr ps -> let envs = List.map (fun p -> check_pattern env p expected) ps in env

    | PAnd ps -> List.fold_left (fun env p -> check_pattern env p expected) env ps



and synth_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty * HopixTypes.typing_environment
  = fun env (Position.{ value = p; position = pos; } as pat) ->
   let _ = ignore (check_pattern_linearity [] pat) in

  match p with
  | PWildcard ->
    (HopixTypes.ATyVar (TId "_a"), env)

  | PVariable id ->
    let ty = HopixTypes.ATyVar (TId "_a") in
    let env' = HopixTypes.bind_value (Position.value id) (Scheme([], ty)) env in
    (ty, env')

  | PLiteral lit ->
    (synth_literal lit.value, env)

  | PTypeAnnotation (p_inner, ty) ->
    let ty = HopixTypes.internalize_ty env ty in
    let env' = check_pattern env p_inner ty in
    (ty, env')

  | PTuple ps ->
    let (tys, env') =
      List.fold_left (fun (acc_tys, acc_env) p_i ->
        let (ty_i, env_i) = synth_pattern acc_env p_i in
        (acc_tys @ [ty_i], env_i)
      ) ([], env) ps
    in
    (HopixTypes.hprod tys, env')

  | PTaggedValue (k, tyargs_opt, ps) ->
    let k_ = Position.value k in
    let Scheme (params, aty) = HopixTypes.lookup_type_scheme_of_constructor pos k_ env in
    let aty_inst =
      match tyargs_opt with
      | None ->
        let fresh_args = List.map (fun _ -> HopixTypes.ATyVar (TId "_a")) params in
        HopixTypes.instantiate_type_scheme (Scheme(params, aty)) fresh_args
      | Some ty ->
        let given = List.map (fun t -> HopixTypes.internalize_ty env t) ty in
        HopixTypes.instantiate_type_scheme (Scheme(params, aty)) given
    in
    let arg_tys, _res = HopixTypes.destruct_function_type_maximally pos aty_inst in
    if List.length arg_tys <> List.length ps then
      HopixTypes.type_error pos "Arity mismatch in tagged-value pattern (synth_pattern)";
    let env' = List.fold_left2 (fun acc_env p_i arg_ty ->
      check_pattern acc_env p_i arg_ty
    ) env ps arg_tys in
    (aty_inst, env')

  | PRecord (fields, tyargs_opt) ->
    let tys_in_order, env' =
      List.fold_left (fun (acc_tys, acc_env) (_lbl, p_i) ->
        let (ty_i, env_i) = synth_pattern acc_env p_i in
        (acc_tys @ [ty_i], env_i)
      ) ([], env) fields
    in
    (HopixTypes.hprod tys_in_order, env')

  | POr ps ->
     (match ps with
      | [] -> HopixTypes.type_error pos "Or-pattern vide"
      | p0::rest ->
         let (ty0, env0) = synth_pattern env p0 in
         
         List.iter (fun p_i ->
             let (ty_i, _env_i) = synth_pattern env p_i in
             if ty_i <> ty0 then
               HopixTypes.type_error pos "Les alternatives d'un or-pattern doivent avoir le même type";
         ) rest;
         (ty0, env0)
     )

  | PAnd ps ->
    let (tys, env') =
      List.fold_left (fun (acc_tys, acc_env) p_i ->
        let (ty_i, env_i) = synth_pattern acc_env p_i in
        (acc_tys @ [ty_i], env_i)
      ) ([], env) ps
    in
    (HopixTypes.hprod tys, env')



let rec synth_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty
  = fun env Position.{ value = e; position = pos; } ->
  match e with
  | Literal l ->
    synth_literal l.value

  | Variable (id, tyargs_opt) ->
    begin
      try
        let scheme =
          HopixTypes.lookup_type_scheme_of_identifier pos id.value env
        in
        match tyargs_opt with
        | Some ty ->
            let given =
              List.map (fun t ->
                HopixTypes.internalize_ty env t
              ) ty
            in
            HopixTypes.instantiate_type_scheme scheme given

        | None ->
            let HopixTypes.Scheme (params, _) = scheme in
            let fresh_args =
              List.map (fun _ -> HopixTypes.ATyVar (TId "_a")) params (*TODO tempo à modif *)
            in
            HopixTypes.instantiate_type_scheme scheme fresh_args

      with HopixTypes.Unbound (p, binding) ->
        HopixTypes.type_error p
        (Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding binding))
    end

  | Apply (f, arg) ->
    begin match f.value with
    | Apply (f2, arg1) ->
        begin match f2.value with
        | Variable (id_op, _) ->
            let raw = match Position.value id_op with Id s -> s in
            let op = normalize_op_name raw in
            let t1 = synth_expression env arg1 in
            let t2 = synth_expression env arg in

            if List.mem op ["+";"-";"*";"/";"%"] then (
              check_equal_types pos ~expected:HopixTypes.hint ~given:t1;
              check_equal_types pos ~expected:HopixTypes.hint ~given:t2;
              HopixTypes.hint
            )
            else if List.mem op ["=";"<>";"<";">";"<=";">="] then (
              check_equal_types pos ~expected:HopixTypes.hint ~given:t1;
              check_equal_types pos ~expected:HopixTypes.hint ~given:t2;
              HopixTypes.hbool
            ) else
              let t1_str = HopixTypes.string_of_aty t1 in
HopixTypes.type_error pos
  (Printf.sprintf
     "This expression has type %s which should be a function type."
     t1_str)



(*"Unknown operator '%s'" raw)*)
        | _ -> HopixTypes.type_error pos "Application non gérée"
        end

    | Variable (id, _) ->
        let raw = match Position.value id with Id s -> s in
        let op = normalize_op_name raw in
        if op = "-" then
          let t = synth_expression env arg in
          check_equal_types pos ~expected:HopixTypes.hint ~given:t;
          HopixTypes.hint
        else

          let fty = synth_expression env f in
          let (dom, cod) = HopixTypes.destruct_function_type pos fty in
          check_expression env arg dom;
          cod

    | _ ->

        let fty = synth_expression env f in
        let (dom, cod) = HopixTypes.destruct_function_type pos fty in
        check_expression env arg dom;
        cod
    end

    | Case (scrutinee, branches) ->
    let t_scrut = synth_expression env scrutinee in
    (match branches with
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
    )

    | Fun (FunctionDefinition (pat, expr)) ->
      let (domain_ty, param_env) = synth_pattern env pat in
      let codomain_ty = synth_expression param_env expr in
      HopixTypes.ATyArrow (domain_ty, codomain_ty)

  | _ ->
    HopixTypes.type_error pos "synth_expression: cas non implémenté"


and check_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty ->
      unit
  = fun env (Position.{ value = e; position = pos; } as exp) expected ->
  match e with
  | Define (vdef, expr) ->
    let new_env = check_value_definition env vdef in
    check_expression new_env expr expected

  | _ ->
    let inferred = synth_expression env exp in
    check_equal_types pos ~expected ~given:inferred

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

  (* collect scheme *)
  let triplets =
    List.map (fun (id, ts, fundef) ->
      match ts with
      | None -> failwith "Type annotation is missing for recursive function"
      | Some (Position.{ value = e; position = pos; } as tys) ->
          let (s, local_ty_env) = check_type_scheme env pos e in
          (id, s, fundef, local_ty_env)
    ) recfun
  in

  (* create env *)
  let fun_env =
    List.fold_left (fun env_acc (id, s, _, _) ->
      HopixTypes.bind_value (Position.value id) s env_acc
    ) env triplets
  in

  (* verif function body *)
  List.iter (fun (id, s, fundef, local_ty_env) ->
    let check_env =
      List.fold_left (fun env_acc (id_other, s_other, _, _) ->
        HopixTypes.bind_value (Position.value id_other) s_other env_acc
      ) local_ty_env triplets
    in
    let HopixTypes.Scheme(_, aty) = s in
    let (in_ts, out_ty) =
      HopixTypes.destruct_function_type_maximally (Position.position id) aty
    in
    let FunctionDefinition(p, expr) = fundef in
    let in_aty = match in_ts with
    | [one] -> one
    | many  -> HopixTypes.hprod many
    in
    let param_env = check_pattern check_env p in_aty in
    check_expression param_env expr out_ty
  ) triplets;
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
