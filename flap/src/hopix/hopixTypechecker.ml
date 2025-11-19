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
  = fun vars Position.{ value; position; } ->
  failwith "Students! This is your job!"

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
  = fun env Position.{ value = p; position = pos; } ->
   failwith "Students! This is your job!"


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
        | Some ty_locs ->
            (* instanciation explicite *)
            let given =
              List.map (fun tloc ->
                HopixTypes.internalize_ty env tloc
              ) ty_locs
            in
            HopixTypes.instantiate_type_scheme scheme given

        | None ->
            (* instanciation polymorphe fraîche *)
            let HopixTypes.Scheme (params, _) = scheme in
            let fresh_args =
              List.map (fun _ -> HopixTypes.ATyVar (TId "_a")) params (*TODO tempo à modif *)
            in
            HopixTypes.instantiate_type_scheme scheme fresh_args

      with HopixTypes.Unbound (p, _) ->
        let Id raw = id.value in
        HopixTypes.type_error p (Printf.sprintf "Unbound identifier `%s'." raw)
    end

  | Apply (f_loc, arg_loc) ->
    begin match f_loc.value with
    | Apply (f2_loc, arg1_loc) ->
        begin match f2_loc.value with
        | Variable (id_op_loc, _) ->
            let raw = match Position.value id_op_loc with Id s -> s in
            let op = normalize_op_name raw in
            let t1 = synth_expression env arg1_loc in
            let t2 = synth_expression env arg_loc in

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

    | Variable (id_loc, _) ->
        let raw = match Position.value id_loc with Id s -> s in
        let op = normalize_op_name raw in
        if op = "-" then
          let t = synth_expression env arg_loc in
          check_equal_types pos ~expected:HopixTypes.hint ~given:t;
          HopixTypes.hint
        else

          let fty = synth_expression env f_loc in
          let (dom, cod) = HopixTypes.destruct_function_type pos fty in
          (* vérifier l'argument *)
          check_expression env arg_loc dom;
          cod

    | _ ->

        let fty = synth_expression env f_loc in
        let (dom, cod) = HopixTypes.destruct_function_type pos fty in
        check_expression env arg_loc dom;
        cod
    end

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
  (* Create a new env with every id initialized and an acc with its linked scheme *)
  let (pairs, new_env) = List.fold_left (fun (acc, env_acc) (id, ts, fundef) ->
    match ts with
    | None -> failwith "a"
    | Some (Position.{ value = e; position = pos; } as tys) ->
      let (s, new_env_) = check_type_scheme env_acc pos e in
      ((id, s)::acc, new_env_)
  ) ([], env) recfun in

  (* Reverse the list of pairs to get them in the right order *)
  let pairs = List.rev pairs in

  (* Bind every pairs together *)
  let fun_env = List.fold_left (fun env_acc (id, s) ->
    HopixTypes.bind_value (Position.value id) s env_acc
  ) new_env pairs in

  (* Iter on each pairs with the new binded env with each function *)
  List.iter (fun (id, s) ->
    let HopixTypes.Scheme(_, aty) = s in
    (* Separate args type and return value type *)
    let (in_ts, out_ty) =
      HopixTypes.destruct_function_type_maximally (Position.position id) aty
    in
    (* Find the corresponding function *)
    let (_, _, fundef) =
      try List.find (fun (id2, _, _) -> id2 = id) recfun
      with Not_found -> (* Supposedly will never happen *)
        failwith "Internal error: function definition not found"
    in
    let FunctionDefinition(p, expr) = fundef in
    (* we need the type of the param of the function,
       using hprod to flatten the list. *)
    let in_aty = match in_ts with
    | [one] -> one
    | many  -> HopixTypes.hprod many
    in
    (* Check if the patterns are correct for the input *)
    let param_env = check_pattern fun_env p in_aty in
    (* Same for the expression with the output *)
    check_expression param_env expr out_ty
  ) pairs;
  (* Return the env with the function, we don't need the
     env with the content of the functions *)
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
