open Opal
open Types

let reserved = ["let"; "in"; "λ"; "#"; "∀"; "Π"]

let initial = letter <|> exactly '_'
let subseqt = alpha_num <|> exactly '_'
let ident = (spaces >> initial <~> many subseqt) => implode >>= function
  | s when List.mem s reserved -> mzero
  | s -> return s

(* let parens = between (token "(") (token ")") *)
let rec to_string l =
  match l with
  | [] -> ""
  | head :: tail ->  head ^ "," ^ (to_string tail)

let list_to_string l =  "[" ^ (to_string l) ^ "]"

let fst (a, _) = a

let rec find_rec pred list i=
  match list with
  | [] -> None
  | head :: tail -> if (pred head i) then (Some head) else find_rec pred tail (i + 1)

let find pred list = find_rec pred list 0

let parens p =
  fun input ->
  (token "(" >>
     p >>= fun result ->
   token ")" >>
     return result) input

let rec term_parser input =
  (type_parser
   <|> pi_parser
   <|> forall_parser
   <|> lam_parser
   <|> app_parser
   <|> let_parser
   <|> ann_parser
   <|> var_parser
   <|> parens term_parser) input

and type_parser input =
  (token "*" >> return (fun _ -> Typ)) input
   
and pi_parser input =
   ((token "Π" <|> token "%") >>
    (option "" ident) >>= fun self ->
    token "("          >>
    (option "" ident) >>= fun name ->
    token ":"          >>
    term_parser >>= fun bind ->
    token ")"          >>
    term_parser >>= fun body ->
    return (fun ctx ->
      All (true, self, name, bind ctx,
           fun s -> (fun x ->
                    body ((name, x)::(self, s):: ctx))))) input

and forall_parser input =
  ((token "∀" <|> token "@") >>
   (option "" ident) >>= fun self ->
   token "("          >>
   (option "" ident) >>= fun name ->
   token ":"          >>
   term_parser >>= fun bind ->
   token ")"          >>
   term_parser >>= fun body ->
   return (fun ctx ->
     All (false, self, name, bind ctx,
          fun s -> fun x -> body ((name, x) :: (self, s) :: ctx)))) input

and lam_parser input =
   ((token "#" <|> token "λ") >>
     ident       >>= fun var ->
     term_parser >>= fun body ->
     return (fun ctx ->
       Lam (var, fun x -> (body ((var, x) :: ctx))))) input

and app_parser input =
  (token "(" >>
   term_parser >>= fun rator ->
   term_parser >>= fun rand  ->
   token ")"   >>
   return (fun ctx ->
     App (rator ctx, rand ctx))) input

and let_parser input =
  (token "let" >>
   ident >>= fun name ->
   token "="   >>
   term_parser >>= fun expr ->
   token "in"  >>
   term_parser >>= fun body ->
   return (fun ctx ->
     Let (name, expr ctx, fun x -> body ((name, x) :: ctx)))) input

and ann_parser input =
  (token "{" >>
   term_parser >>= fun expr ->
   token ":" >>
   term_parser >>= fun typp ->
   token "}" >>
   return (fun ctx ->
     Ann (false, expr ctx, typp ctx))) input

and var_parser input =
  (ident >>= fun name ->
   return (fun ctx ->
       let pred = fun x -> fun _ -> String.equal (fst x) name in
       let index = find pred ctx in
       match index with
       | None -> Ref name
       | Some (_, term) -> term)) input

let parse_def input =
  (ident >>= fun name ->
   token ":"          >>
   term_parser >>= fun typp ->
   token "="          >>
   term_parser >>= fun body ->
   token ";"          >>
   return (Def (name, typp [], body []))) input

let parse_defs input = (many1 parse_def) input 
