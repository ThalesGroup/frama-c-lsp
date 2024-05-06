open Types
open Find_def 
open Utils
  
let rq_handler json_string () =

  let json = Json.load_string json_string in 
  let request = RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 

  match curr_method with 
  | "textDocument/definition" -> Db.Main.extend (find_def (Json.save_string (get request.params)));
  | _ -> Printf.printf "Nope";

  let request_json = Json.save_string json in
  Printf.printf "test7\nsave_string\n%s\n" json_string;
  Printf.printf "load string\n%s\n" request_json;
  Printf.printf "method\n%s\n" curr_method
