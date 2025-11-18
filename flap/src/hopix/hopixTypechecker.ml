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
  (Scheme(ts, HopixTypes.internalize_ty env ty), new_env)

let expect_int pos aty =
  if aty <> HopixTypes.hint then
    HopixTypes.type_error pos
      (Printf.sprintf "Attendu int, obtenu : %s" (HopixTypes.string_of_aty aty))

let expect_bool pos aty =
  if aty <> HopixTypes.hbool then
    HopixTypes.type_error pos
      (Printf.sprintf "Attendu bool, obtenu : %s" (HopixTypes.string_of_aty aty))


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
  failwith "Students! This is your job!"

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

  | Variable (id, None) ->
    begin try
      let Scheme(ts, ty) =
        HopixTypes.lookup_type_scheme_of_identifier pos id.value env
      in ty
    with HopixTypes.Unbound (pos, _) ->
      let Id i = id.value in
      (* TODO: Demander si la syntaxe ici est normale *)
      HopixTypes.type_error pos (Printf.sprintf "Unbound identifier `%s'." i);
    end

  | Apply (f_loc, arg2_loc) ->
    begin match f_loc.value with

    | Apply (f2_loc, arg1_loc) ->
      begin match f2_loc.value with
      | Variable (id_op_loc, _) ->
        let raw = match Position.value id_op_loc with Id s -> s in
        let op = normalize_op_name raw in

        let t1 = synth_expression env arg1_loc in
        let t2 = synth_expression env arg2_loc in

        if List.mem op ["+";"-";"*";"/";"%"] then (
          check_equal_types pos ~expected:HopixTypes.hint ~given:t1;
          check_equal_types pos ~expected:HopixTypes.hint ~given:t2;
          HopixTypes.hint
        )

        else if List.mem op ["=";"<>";"<";">";"<=";">="] then (
          check_equal_types pos ~expected:HopixTypes.hint ~given:t1;
          check_equal_types pos ~expected:HopixTypes.hint ~given:t2;
          HopixTypes.hbool
        )

        else
          HopixTypes.type_error pos (Printf.sprintf "Unknown operator '%s'" raw)

      | _ -> HopixTypes.type_error pos "Application non gérée"
      end

    | Variable (id_loc, _) ->
      let raw = match Position.value id_loc with Id s -> s in
      let op = normalize_op_name raw in
      if op = "-" then
        let t = synth_expression env arg2_loc in
        check_equal_types pos ~expected:HopixTypes.hint ~given:t;
        HopixTypes.hint
      else
        HopixTypes.type_error pos
          (Printf.sprintf "Application non-implémentée pour '%s'" raw)

    | _ ->
      HopixTypes.type_error pos "Application complexe non gérée"
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
  (* Create a new env with every id initialized *)
  let new_env = List.fold_left (fun env (id, ts, fundef) ->
    match ts with
    | None -> failwith "a"
    | Some (Position.{ value = e; position = pos; } as tys) ->
      let (s, new_env) = check_type_scheme env pos e in
      (* Marche pas je sais pas pk *)
      HopixTypes.bind_value id.value s new_env
  ) env recfun in
  new_env

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
