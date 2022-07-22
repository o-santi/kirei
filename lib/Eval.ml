open Base

type 'a reply =
  | Value of 'a
  | Error of string

let (let+) x f =
  match x with
  | Error msg -> Error msg
  | Value v -> f v

type expr =
  | EVar of string
  | EApp of expr * expr
  | ELam of string * expr
  | ELet of string * expr * expr

type value =
  | VVar of string
  | VApp of value * value
  | VLam of string * (value -> value reply)

(* this is stupid *)
(* i should write a proper environment-associated gen-sym *)
let make_fresh =
  let id_counter = ref (-1) in
  let gen_id = fun () -> Int.incr id_counter; !id_counter in
  let freshen name = name ^ "_" ^ (Int.to_string (gen_id ())) in
  freshen


let lookup env sym =
  match Map.find env sym with
  | None -> Error ("Variable " ^ sym ^ " is not defined.")
  | Some v -> Value v

let rec eval env expr =
  match expr with
  | EVar name -> lookup env name
  | EApp (func, args) -> 
     let+ rator = eval env func in
     let+ rand  = eval env args in
     begin
       match rator with
       | VLam (_, f) -> f rand
       | VApp (_, _) -> Value (VApp (rator, rand))
       | VVar _      -> Value (VApp (rator, rand))
     end
  | ELam (name, body) ->
     let lambda = fun u -> eval (Map.set env ~key:name ~data:u) body in
     Value (VLam (name, lambda))
  | ELet (name, exp, body) ->
     let+ vall = eval env exp in
     let new_env = Map.set env ~key:name ~data:vall in
     eval new_env body

and quote gen_sym repv =
  match repv with
  | Error msg -> Error msg
  | Value (VVar name) -> Value (EVar name)
  | Value (VApp (f, args)) ->
     let+ quote_f    = quote gen_sym (Value f) in
     let+ quote_args = quote gen_sym (Value args) in
     Value (EApp (quote_f, quote_args))
  | Value (VLam (name, body)) ->
     let new_name = gen_sym name in
     let+ quote_lam = quote gen_sym (body (VVar new_name)) in
     Value (ELam (new_name, quote_lam))

let rec show term =
  match term with
  | EVar name     -> name
  | EApp (func, arg) -> "(" ^ (show func) ^ " " ^ (show arg) ^ ")"
  | ELam (var, body) -> "(λ" ^ var ^ " " ^ (show body) ^ ")"
  | ELet (var, exp, body) -> "let " ^ var ^ " = " ^ (show exp) ^ " in\n" ^ (show body)

let normalize env expr =
  let v = eval env expr in
  let gen_sym = make_fresh in
  quote gen_sym v

let run code =
  let norm = normalize (Map.empty (module String)) code in
  match norm with
  | Error msg -> msg
  | Value term -> (show term)
