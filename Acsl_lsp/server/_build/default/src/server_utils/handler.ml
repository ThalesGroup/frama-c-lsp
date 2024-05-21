open Types
open Initialize
open Find_def
  
let rq_handler json_string =

  let json = Json.load_string json_string in 
  let request = RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  Printf.printf "handler\n%!";

  match curr_method with 
  | "initialize" -> 
    initialize request;
  | "textDocument/definition" -> 
    find_def (Ast.get ()) request;
  | _ -> `Null;
