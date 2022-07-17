open Opal
open Eval

let reserved = ["let"; "in"; "λ"; "#"]

let initial = letter <|> exactly '_'
let subseqt = alpha_num <|> exactly '_'
let ident = (spaces >> initial <~> many subseqt) => implode >>= function
  | s when List.mem s reserved -> mzero
  | s -> return s

let parens = between (token "(") (token ")")

let var_parser input =
  (ident >>= fun name ->
   spaces >>
   return (EVar name)) input

let rec my_fold f base list =
  match list with
  | [] -> base
  | h :: t -> my_fold f (f base h) t

let rec expr_parser input =
  (let_parser <|> lam_parser <|> app_parser) input
and let_parser input =
  (token "let" >>
   ident       >>= fun name ->
   token "="   >>
   expr_parser >>= fun exp  ->
   token "in"  >>
   expr_parser >>= fun body ->
   return (ELet (name, exp, body))) input

and lam_parser input =
  ((token "#" <|> token "λ") >>
   ident        >>= fun var ->
   expr_parser  >>= fun body ->
   return (ELam (var, body))) input

and app_parser input =
  (atom_parser >>= fun func_name ->
   many atom_parser >>= fun args ->
   let f = fun base -> fun step -> EApp (base, step) in
   return (my_fold f func_name args)) input

and atom_parser input =
  (var_parser <|> parens expr_parser) input
