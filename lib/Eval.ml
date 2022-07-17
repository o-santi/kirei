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

let rec freshen name list =
  match (List.mem list name ~equal:String.equal) with
  | true -> freshen (name ^ "-") list
  | false -> name

let lookup env sym =
  match Map.find env sym with
  | None -> Error ("A variavel " ^ sym ^ " não foi definida.")
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

and quote used repv =
  match repv with
  | Error msg -> Error msg
  | Value (VVar name) -> Value (EVar name)
  | Value (VApp (f, args)) ->
     let+ quote_f    = quote used (Value f) in
     let+ quote_args = quote used (Value args) in
     Value (EApp (quote_f, quote_args))
  | Value (VLam (name, body)) ->
     let new_sym = freshen name used in
     let+ quote_lam = quote (name :: used) (body (VVar new_sym)) in
     Value (ELam (new_sym, quote_lam))

let normalize env expr =
  let v = eval env expr in
  quote (Map.keys env) v

let rec show term =
  match term with
  | EVar name     -> name
  | EApp (func, arg) -> "(" ^ (show func) ^ " " ^ (show arg) ^ ")"
  | ELam (var, body) -> "(λ" ^ var ^ " " ^ (show body) ^ ")"
  | ELet (var, exp, body) -> "let " ^ var ^ " = " ^ (show exp) ^ " in\n" ^ (show body)

let run code =
  let norm = normalize (Map.empty (module String)) code in
  match norm with
  | Error msg -> msg
  | Value term -> (show term)
