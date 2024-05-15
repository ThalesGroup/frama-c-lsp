open Types
open Initialize
open Find_def
open Utils
  
let rq_handler json_string () =

  let json = Json.load_string json_string in 
  let request = RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  Printf.printf "handler\n%!";

  match curr_method with 
  | "initialize" -> 
    let params = InitializeParams.t_of_json (get request.params) in
    initialize params;
  | "textDocument/definition" -> 
    let params = DefinitionParams.t_of_json (get request.params) in
    find_def (Ast.get ()) params;
  | _ -> Printf.printf "None\n%!";
