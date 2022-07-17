open Kirei.Parser
open Kirei.Eval
open Opal
open Stdio

let () =
  let code = In_channel.read_all "example.ki" in
  match expr_parser (LazyStream.of_string code) with
  | None -> print_endline "Syntax error."
  | Some (term,_) -> print_endline (run term)
