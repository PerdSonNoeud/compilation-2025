open Position
open Error
open HopixAST

(** [error pos msg] reports execution error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of Hopix evaluates into a [value].

   The [value] type is not defined here. Instead, it will be defined
   by instantiation of following ['e gvalue] with ['e = environment].
   Why? The value type and the environment type are mutually recursive
   and since we do not want to define them simultaneously, this
   parameterization is a way to describe how the value type will use
   the environment type without an actual definition of this type.

*)
type 'e gvalue =
  | VInt       of Mint.t
  | VChar      of char
  | VString    of string
  | VUnit
  | VTagged    of constructor * 'e gvalue list
  | VTuple     of 'e gvalue list
  | VRecord    of (label * 'e gvalue) list
  | VLocation  of Memory.location
  | VClosure   of 'e * pattern located * expression located
  | VPrimitive of string * ('e gvalue Memory.t -> 'e gvalue -> 'e gvalue)

(** Two values for booleans. *)
let ptrue  = VTagged (KId "True", [])
let pfalse = VTagged (KId "False", [])

(**
    We often need to check that a value has a specific shape.
    To that end, we introduce the following coercions. A
    coercion of type [('a, 'e)] coercion tries to convert an
    Hopix value into a OCaml value of type ['a]. If this conversion
    fails, it returns [None].
*)

type ('a, 'e) coercion = 'e gvalue -> 'a option
let fail = None
let ret x = Some x
let value_as_int      = function VInt x -> ret x | _ -> fail
let value_as_char     = function VChar c -> ret c | _ -> fail
let value_as_string   = function VString s -> ret s | _ -> fail
let value_as_tagged   = function VTagged (k, vs) -> ret (k, vs) | _ -> fail
let value_as_record   = function VRecord fs -> ret fs | _ -> fail
let value_as_location = function VLocation l -> ret l | _ -> fail
let value_as_closure  = function VClosure (e, p, b) -> ret (e, p, b) | _ -> fail
let value_as_primitive = function VPrimitive (p, f) -> ret (p, f) | _ -> fail
let value_as_bool = function
  | VTagged (KId "True", []) -> true
  | VTagged (KId "False", []) -> false
  | _ -> assert false

(**
   It is also very common to have to inject an OCaml value into
   the types of Hopix values. That is the purpose of a wrapper.
 *)
type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x
let bool_as_value b = if b then ptrue else pfalse

(**

  The flap toplevel needs to print the result of evaluations. This is
   especially useful for debugging and testing purpose. Do not modify
   the code of this function since it is used by the testsuite.

*)
let print_value m v =
  (** To avoid to print large (or infinite) values, we stop at depth 5. *)
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
          Mint.to_string x
        | VChar c ->
          "'" ^ Char.escaped c ^ "'"
        | VString s ->
          "\"" ^ String.escaped s ^ "\""
        | VUnit ->
          "()"
        | VLocation a ->
          print_array_value d (Memory.dereference m a)
        | VTagged (KId k, []) ->
          k
        | VTagged (KId k, vs) ->
          k ^ print_tuple d vs
        | VTuple (vs) ->
           print_tuple d vs
        | VRecord fs ->
           "{"
           ^ String.concat ", " (
                 List.map (fun (LId f, v) -> f ^ " = " ^ print_value (d + 1) v
           ) fs) ^ "}"
        | VClosure _ ->
          "<fun>"
        | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
    and print_tuple d vs =
      "(" ^ String.concat ", " (List.map (print_value (d + 1)) vs) ^ ")"
    and print_array_value d block =
      let r = Memory.read block in
      let n = Mint.to_int (Memory.size block) in
      "[ " ^ String.concat ", " (
                 List.(map (fun i -> print_value (d + 1) (r (Mint.of_int i)))
                         (ExtStd.List.range 0 (n - 1))
               )) ^ " ]"
  in
  print_value 0 v

let print_values m vs =
  String.concat "; " (List.map (print_value m) vs)

module Environment : sig
  (** Evaluation environments map identifiers to values. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)
  val bind    : t -> identifier -> t gvalue -> t

  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)
  val update  : Position.t -> identifier -> t -> t gvalue -> unit

  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)
  val lookup  : Position.t -> identifier -> t -> t gvalue

  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)
  exception UnboundIdentifier of identifier * Position.t

  (** [last env] returns the latest binding in [env] if it exists. *)
  val last    : t -> (identifier * t gvalue * t) option

  (** [print env] returns a human readable representation of [env]. *)
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v =
    lookup' pos x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding m (Id x, v) =
    x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

(**
    We have everything we need now to define [value] as an instantiation
    of ['e gvalue] with ['e = Environment.t], as promised.
*)
type value = Environment.t gvalue

(**
   The following higher-order function lifts a function [f] of type
   ['a -> 'b] as a [name]d Hopix primitive function, that is, an
   OCaml function of type [value -> value].
*)
let primitive name ?(error = fun () -> assert false) coercion wrapper f
: value
= VPrimitive (name, fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

type runtime = {
  memory      : value Memory.t;
  environment : Environment.t;
}

type observable = {
  new_memory      : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    let error m v =
      Printf.eprintf
        "Invalid arguments for `%s': %s\n"
        name (print_value m v);
      assert false (* By typing. *)
    in
    VPrimitive (name, fun m -> function
      | VInt x ->
         VPrimitive (name, fun m -> function
         | VInt y -> out (op x y)
         | v -> error m v)
      | v -> error m v)
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v))
      x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name =
    intbin name (fun x -> VInt x) in
  let binarithops = Mint.(
    [ ("`+`", add); ("`-`", sub); ("`*`", mul); ("`/`", div) ]
  ) in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name bool_as_value in
  let cmparithops =
    [ ("`=?`", ( = ));
      ("`<?`", ( < ));
      ("`>?`", ( > ));
      ("`>=?`", ( >= ));
      ("`<=?`", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, fun _ x -> VPrimitive (name, fun _ y ->
        out (op (value_as_bool x) (value_as_bool y))))
  in
  let boolarith name = boolbin name (fun x -> if x then ptrue else pfalse) in
  let boolarithops =
    [ ("`||`", ( || )); ("`&&`", ( && )) ]
  in
  let generic_printer =
    VPrimitive ("print", fun m v ->
      output_string stdout (print_value m v);
      flush stdout;
      VUnit
    )
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive  ("print_int", fun _ -> function
      | VInt x -> print (Mint.to_string x)
      | _ -> assert false (* By typing. *)
    )
  in
  let print_string =
    VPrimitive  ("print_string", fun _ -> function
      | VString x -> print x
      | _ -> assert false (* By typing. *)
    )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print"        generic_printer
  |> bind' "print_int"    print_int
  |> bind' "print_string" print_string
  |> bind' "true"         ptrue
  |> bind' "false"        pfalse
  |> bind' "nothing"      VUnit

let initial_runtime () = {
  memory      = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
  environment = primitives;
}

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(** [definition pos runtime d] evaluates the new definition [d]
    into a new runtime [runtime']. In the specification, this
    is the judgment:

                        E, M ⊢ dv ⇒ E', M'

*)
and definition runtime d =
  match d.value with
  | DefineValue(vdef) -> value_definition vdef runtime
  | _                 -> runtime

and value_definition vdef runtime =
  match vdef with
  | SimpleValue(pdef)    -> simp_val runtime pdef
  | RecFunctions(fundef) -> rec_fun runtime fundef

and simp_val runtime pdef =
  let env = runtime.environment in
  let mem = runtime.memory in
  let (id, _, expr) = pdef in
  let (v, new_mem) = expression' env mem expr in
  let new_env = Environment.bind env id.value v in
  { environment = new_env; memory = new_mem }

and rec_fun runtime fundef =
  let env = runtime.environment in
  let mem = runtime.memory in
  match fundef with
  | h::t ->
      let (id, _, a) = h in
      let FunctionDefinition(_, expr) = a in
      let (v, new_mem) = expression' env mem expr in
      let new_env = Environment.bind env id.value v in
      rec_fun { environment = new_env; memory = new_mem } t
  | []   -> runtime

and expression' environment memory e =
  expression (position e) environment memory (value e)

(** [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression _ environment memory = function
  | Literal (pos)  -> literal pos memory

  | Variable (id, _) ->
    let v = Environment.lookup id.position id.value environment in
    (v, memory)

  | Apply (e1, e2) ->
    apply environment memory e1 e2

  | Define (vd, e) ->
    let runtime = value_definition vd { environment; memory } in
    expression' runtime.environment runtime.memory e

  | Fun(FunctionDefinition(p, expr)) ->
    (VClosure (environment, p, expr), memory)

  | IfThenElse (c, t, f) ->
    let (v, _) = expression' environment memory c in
    ifThenElse environment memory v t f

  | Tuple es ->
    let rec eval_all env mem = function
        | [] -> ([], mem)
        | e :: xs ->
            let (v, mem1) = expression e.position env mem e.value in
            let (vs, mem2) = eval_all env mem1 xs in
                (v :: vs, mem2)
    in let (vs, mem') = eval_all environment memory es in
        (VTuple vs, mem')

  | Tagged (constructor, _, exprs) ->
        let rec eval_list env mem = function
            | [] -> ([], mem)
            | e :: xs ->
                let (v, mem1) = expression' env mem e in
                    let (vs, mem2) = eval_list env mem1 xs in
                        (v :: vs, mem2)
        in let (values, mem') = eval_list environment memory exprs in
            (VTagged (Position.value constructor, values), mem')

  | Record (fields, _) ->
    let rec eval_fields env mem = function
      | [] -> ([], mem)
      | (label, expr) :: xs ->
          let (v, mem1) = expression' env mem expr in
          let (vs, mem2) = eval_fields env mem1 xs in
            ((Position.value label, v) :: vs, mem2)
    in let (evaluated_fields, mem') = eval_fields environment memory fields in
    (VRecord evaluated_fields, mem')

  | Field (e, label, _) ->
    let (v, mem') = expression' environment memory e in
    (match v with
     | VRecord fields ->
         let lbl = Position.value label in
         (try let value = List.assoc lbl fields in (value, mem')
          with Not_found -> failwith ("Field not found"))
     | _ -> failwith "Field sur un non record")

  | Sequence expr ->
    let rec eval_seq env mem = function
      | [] -> failwith "sequence vide"
      | [last] -> expression' env mem last
      | e :: xs ->
          let (_, mem1) = expression' env mem e in
          eval_seq env mem1 xs
    in eval_seq environment memory expr

  (*il faut les ref pour tester peut être que le while marche pas'*)
  | While(e1,e2) -> print_endline "test";
    let (v,mem) = expression' environment memory e1 in 
        begin match value_as_bool v with
            | false -> (VUnit,memory)
            | true -> let (_,m') = expression' environment memory e2 
                in let e = { value = While(e1,e2); position = e1.position } in expression' environment m' e
        end

  | _ -> failwith "Students! This is your job!___"

and literal pos memory =
  match pos.value with
  | LInt(x) -> (VInt x, memory)
  | LChar(x) -> (VChar x, memory)
  | LString(x) -> (VString x, memory)

and apply env mem e1 e2 =
  let (v1, mem1) = expression' env mem e1 in
  let (v2, mem2) = expression' env mem1 e2 in
  match v1 with
  | VClosure (env_, p, expr) ->
      let new_env = bind_pattern p v2 env_ in
      expression' new_env mem2 expr

  | VPrimitive (_, f) ->
      let v = f mem v2 in
      (v, mem)

  | _ -> failwith "problème"


and pattern p =
  match p with
  | PVariable (id) -> id.value
  | _ -> failwith "jsp"

and bind_pattern p v env =
  match p.value, v with

  | PVariable id, _ -> Environment.bind env id.value v

  | PWildcard, _ -> env

  | PTypeAnnotation (p, _), _ ->
      bind_pattern p v env

  | PLiteral lit, _ when literal_matches lit.value v -> env
  | PLiteral _, _ -> failwith "problème"

  | PTuple t1, VTuple t2 ->
      bind_list t1 t2 env

  | PTaggedValue (constr, _, p1), VTagged (constr', p2)
    when Position.value constr = constr' -> bind_list p1 p2 env

  | PRecord (fields, _), VRecord vals ->
      List.fold_left (fun e (lbl, p) ->
        let lbl = Position.value lbl in
        let v_field =
          try List.assoc lbl vals with Not_found ->
            failwith "problème"
        in
        bind_pattern p v_field e
      ) env fields

  | POr ou, _ ->
      try_first_match ou v env

  | PAnd et, _ ->
      List.fold_left (fun e p -> bind_pattern p v e) env et

  | _ ->
      failwith "Pattern matching failure"

and literal_matches lit v =
  match lit, v with
  | LInt x, VInt y -> x = y
  | LChar c, VChar d -> c = d
  | LString s, VString t -> s = t
  | _ -> false


and bind_list l1 l2 env =
  if List.length l1 <> List.length l2 then
    failwith "Problème"
  else
    List.fold_left2 (fun e p v -> bind_pattern p v e) env l1 l2


and try_first_match p v env =
  let rec aux = function
    | [] -> failwith "pas de match"
    | p :: rest ->
        try bind_pattern p v env
        with Failure _ -> aux rest
  in
  aux p



and ifThenElse env mem v t f =
  match value_as_bool v with
  | true -> expression' env mem t
  | false -> expression' env mem f

(** This function returns the difference between two runtimes. *)
and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory =
      runtime'.memory
  }

(** This function displays a difference between two runtimes. *)
let print_observable (_ : runtime) observation =
  Environment.print observation.new_memory observation.new_environment
