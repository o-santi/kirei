open Base
(* open Stdio *)

type term =
  | Var of string * int                  (* Var(name, debruijn index *)
  | Ref of string                        (* Ref(name)*)
  | Typ                                  (* Universal type *)
  | All of bool * string * string * term * (term -> term -> term)
                                         (* All(erased, self, name, var_type, body) *)
  | Lam of string * (term -> term)        (* Lam(var, body) *)
  | App of term * term                   (* App(function, argument) *)
  | Let of string * term * (term -> term) (* Let(var, expr, body) *)
  | Ann of bool * term * term            (* Ann(erased, expr, type) *)

type def = Def of string * term * term (* Def(name, type, term) *)
module StrPair = struct
  type t = string * string [@@deriving compare, sexp_of]
end

module Lexicographical_order = struct 
  include StrPair
  include Base.Comparator.Make(StrPair)
end

type 'a reply =
  | Value of 'a
  | Error of string

let (let+) x f =
  match x with
  | Error msg -> Error msg
  | Value v -> f v
     

let rec show term =
  match term with
  | Var (name,_) -> name
  | Ref name   -> name
  | Typ        -> "*"
  | All (erased, self, name, var_type, body) ->
     let bind  = if erased then "∀" else "Π" in
     let var_t = show var_type in
     let bodyy = body (Var (self, 0)) (Var (name, 0)) in
     bind ^ self ^ " (" ^ name ^ ": " ^ var_t ^ ") " ^ show bodyy
  | Lam (name, body) ->
     let body_show = show (body (Var(name, 0))) in
     "#" ^ name ^ " " ^ body_show
  | App (rator, rand) ->
     let func = show rator in
     let args = show rand  in
     "(" ^ func ^ " " ^ args ^ ")"
  | Let (name, expr, body) ->
     let expr_show = show expr in
     let body_show = show (body (Var(name, 0))) in
     "let " ^ name ^ " = " ^ expr_show ^ " in " ^ body_show
  | Ann (_, expr, _) -> show expr

let rec serialize term dep ini =
  match term with
  | Var (_, lvl) ->
     if (lvl >= ini) then
       "^-" ^ (Int.to_string ((dep - lvl) - 1))
     else
       "^+" ^ (Int.to_string lvl)
  | Ref name -> "$" ^ name
  | Typ -> "*"
  | All (erased, self, _, var_type, body) ->
     let init = if erased then "%" else "@" in
     let bind = serialize var_type dep ini in
     let body_s = serialize (body (Var ("", dep)) (Var ("", dep+1))) (dep + 2) ini in
     init ^ self ^ bind ^ body_s
  | Lam (_, body) -> "#" ^ serialize (body (Var ("", dep))) (dep+1) ini 
  | App (func, args) ->
     let funs = serialize func dep ini in
     let argm = serialize args dep ini in
     "(" ^ funs ^  " " ^ argm ^ ")"
  | Let (_,expr,body) ->
     let exp = serialize expr dep ini in
     let bod = serialize (body (Var ("", dep))) (dep + 1) ini in
     "!" ^ exp ^ bod
  | Ann (_, expr, _) -> serialize expr dep ini

let rec reduce term defs =
  match term with
  | Ref name ->
     let search = Map.find defs name in
     begin match search with
     | None -> Ref name
     | Some (Def (_, _, t)) -> t
     end
  | App (rator, rand) ->
     let func = reduce rator defs in
     begin match func with
     | Lam (_ , body) -> reduce (body rand) defs
     | _ -> App (func, rand)
     end
  | Ann (_, expr, _)    -> reduce expr defs
  | Let (_, expr, body) -> reduce (body expr) defs
  | _ -> term


let rec normalize term defs =
  let norm_weak = reduce term defs in
  Stdio.print_endline (show norm_weak);
  match norm_weak with 
  | Let (_, expr, body) -> normalize (body expr) defs
  | Ann (_, expr, _) -> normalize expr defs
  | Lam (name, body) ->
     let new_body = fun x -> normalize (body x) defs in
     Lam (name, new_body)
  | App (rator, rand) ->
     let func = normalize rator defs in
     let args = normalize rand  defs in
     App (func, args)
  | All (erased, self, name, var_type, body) ->
     let x_type = normalize var_type defs in
     let new_body = fun s -> fun x -> normalize (body s x) defs in
     All (erased, self, name, x_type, new_body)
  | t -> t

let rec eval term defs =
  Stdio.print_endline (show term);
  match term with
  | Ref name ->
     let search = Map.find defs name in
     begin match search with
     | None -> Ref name
     | Some (Def (_, _, t)) -> eval t defs
     end
  | Let (_, expr, body) -> eval (body expr) defs
  | Lam (name, body) ->
     let new_body = fun x -> eval (body x) defs in
     Lam (name, new_body)
  | App (rator, rand) ->
     let func = eval rator defs in
     let args = eval rand  defs in
     App (func, args)
  | All _ -> Typ
  (* | All (erased, self, name, var_type, body) -> *)
  (*    if erased then Typ else *)
  (*      let x_type = eval var_type defs in *)
  (*      let body = fun s -> fun x -> eval (body s x) defs in *)
  (*      All (erased, self, name, x_type, body) *)
  | t -> t

let rec equal a b defs depth seen =
  let a1 = reduce a defs in
  let b1 = reduce b defs in
  let a1_serialized = serialize a1 depth depth in
  let b1_serialized = serialize b1 depth depth in
  if (String.equal a1_serialized b1_serialized) ||
       (Set.mem seen (a1_serialized, b1_serialized)) then
    true
  else
    let seen = Set.add seen (a1_serialized, b1_serialized) in
    match (a1, b1) with
    | (Ann (_, exp1, _), Ann (_, exp2, _)) -> equal exp1 exp2 defs depth seen
    | (App (f1, arg1), App (f2, arg2)) -> equal f1 f2 defs depth seen && equal arg1 arg2 defs depth seen
    | (Let (name1, exp1, body1), Let (_, exp2, body2)) ->
       let a1_body = body1 ( Var (name1, depth) ) in
       let b1_body = body2 ( Var (name1, depth) ) in
       equal exp1    exp2    defs (depth + 0) seen &&
         equal a1_body b1_body defs (depth + 1) seen
    | (Lam (var1, body1), Lam(_, body2)) ->
       let a1_body = body1 ( Var (var1, depth) ) in
       let b1_body = body2 ( Var (var1, depth) ) in
       equal a1_body b1_body defs (depth + 1) seen
    | (All (erased1, self1, name1, var_type1, body1), All(erased2, _, _, var_type2, body2)) ->
       let a1_body = body1 (Var (self1, depth)) (Var (name1, depth+1)) in
       let b1_body = body2 (Var (self1, depth)) (Var (name1, depth+1)) in
       (Bool.equal erased1 erased2) &&
         equal var_type1 var_type2 defs depth seen &&
           equal a1_body b1_body defs (depth + 2) seen
    | (Typ, Typ) -> true
    | (Var (_, idx1), Var (_, idx2)) -> idx1 = idx2
    | (Ref name1, Ref name2) -> String.equal name1 name2
    | (_,_) -> false

let rec type_check term typp def_name defs ctx =
  let typev = reduce typp defs in
  match term with
  | Lam (_, lam_body) ->
     begin match typev with
     | All (_, _, name, var_type, body) ->
        let self_var = Ann (true, term, typp) in
        let name_var = Ann (true, Var(name, (List.length ctx) + 1), var_type) in
        let body_typ = body self_var name_var in
        let body_ctx = (name, var_type) :: ctx in
        type_check (lam_body name_var) body_typ def_name defs body_ctx 
     | _ -> Error ("Lambda does not have a function type.\nIn: " ^ def_name ^ "\nfound:" ^ show typev)
     end
  | Let (name, expr, body) ->
     let+ expr_type = type_infer expr def_name defs ctx in
     let expr_var = Ann (true, Var (name, List.length ctx), expr_type) in
     let body_ctx = (name, expr_type) :: ctx in
     type_check (body expr_var) typp def_name defs body_ctx
  | _ ->
     let+ infer = type_infer term def_name defs ctx in
     let eq = equal typp infer defs (List.length ctx) (Set.empty (module Lexicographical_order)) in
     begin match eq with
     | true -> Value term
     | false ->
        let expected = show (normalize typev  (Map.empty (module String))) in
        let infered  = show (normalize infer (Map.empty (module String))) in
        Error ("Expected term: "  ^ show term ^
               "\nto have type: " ^ expected  ^
               "\nbut got: "      ^ infered   ^
               "\n\nIn: "         ^ def_name)
     end

and type_infer term def_name defs ctx =
  match term with
  | Var (name, idx) -> Value (Var (name, idx))
  | Ref name ->
     let definition = Map.find defs name in 
     begin match definition with
     | None -> Error ("Unbound reference " ^ name ^ " in " ^ def_name ^ " : " ^ (show term))
     | Some (Def (_, typ, _)) -> Value typ
     end
  | Typ -> Value Typ
  | App (func, args) ->
     let+ func_infer = type_infer func def_name defs ctx in
     let func_type = reduce func_infer defs in
     begin match func_type with
     | All (_, _, _, var_type, body) ->
        let self_var = Ann (true, func, func_type) in
        let name_var = Ann (true, args, var_type) in
        let+ _ = type_check args var_type def_name defs ctx in
        let term_typ = body self_var name_var in
        Value term_typ
     | _ ->
        Error ("Cannot apply non-function (in definition " ^ def_name ^ ") : " ^ show term)
     end
  | Let (name, expr, body) ->
     let+ expr_typ = type_infer expr def_name defs ctx in
     let  expr_var = Ann (true, Var (name, (List.length ctx) + 1), expr_typ) in
     let  body_ctx = (name, expr_typ) :: ctx in
     type_infer (body expr_var) def_name defs body_ctx
  | All (_, self, name, var_type, body) ->
     let self_var = Ann (true, Var (self, List.length ctx), term) in
     let name_var = Ann (true, Var (name, List.length ctx), var_type) in
     let body_ctx = (self, term) :: (name, var_type) :: ctx in
     let+ _ = type_check var_type Typ def_name defs ctx in
     let+ _ = type_check (body self_var name_var) Typ def_name defs body_ctx in
     Value Typ
  | Ann (don, expr, typ) ->
     if not don then
       type_check expr typ def_name defs ctx
     else
       Value typ
  | _ -> Error ("Can't infer type for " ^ show term ^ "\nIn: " ^ def_name)


let rec check defs list_defs =
  match list_defs with
  | [] -> Value "\nEverything Checks."
  | (name, (Def(_, typ, term))) :: tail ->
     let+ _ = type_check typ Typ name defs [] in
     let+ _ = type_check term typ name defs [] in
     let+ msg = check defs tail in
     Value (name ^ " : " ^ show typ ^ "\n" ^ msg)

    
