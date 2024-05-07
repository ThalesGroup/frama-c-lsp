open Types
open Find_def 
open Utils
  
let rq_handler json_string () =

  let json = Json.load_string json_string in 
  let request = RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 

  match curr_method with 
  | "textDocument/definition" -> find_def (Json.save_string (get request.params));
  | _ -> Printf.printf "Nope\n%!";
