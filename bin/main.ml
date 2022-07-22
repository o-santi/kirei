open Kirei.Parser
open Kirei.Types
open Opal
open Core

let type_check filename =
  let code = In_channel.read_all filename in
  match parse_defs (LazyStream.of_string code) with
  | None -> print_endline "Syntax error." (* TODO: better error message on parsing*)
  | Some (defs,_) ->
     let def_list = List.map defs ~f:(fun def ->
                        match def with
                        | Def (name, _, _) -> (name, def)) in
     let def_map = Map.of_alist_exn (module String) def_list in
     let result = check def_map def_list in
     match result with
     | Error msg -> Stdio.print_endline ("Error:\n" ^ msg)
     | Value msg -> Stdio.print_endline msg

let run filename =
  let code = In_channel.read_all filename in
  match parse_defs (LazyStream.of_string code) with
  | None -> print_endline "Syntax error." (* TODO: better error message on parsing*)
  | Some (defs,_) ->
     let main = List.find defs ~f:(fun def ->
                        match def with
                        | Def (name, _, _) -> String.equal name "main") in
     match main with
     | None -> Stdio.print_endline ("Error: main not found")
     | Some (Def (_, _, term)) ->
        let def_list = List.map defs ~f:(fun def ->
                           match def with
                           | Def (name, _, _) -> (name, def))in
        let def_map = Map.of_alist_exn (module String) def_list in
        let result = normalize term def_map in
        Stdio.print_endline (show result)

let () =
  Command.basic
    ~summary:"Check or run file"
    [%map_open.Command
      let mode = anon ("mode" %: string) 
      and filename = anon ("filename" %: string) in 
          fun () ->
          match mode with
          | "check" -> type_check filename
          | "run" -> run filename 
          | other -> Stdio.print_endline (other ^ " is not a valid mode")
        ]
  |> Command.run
