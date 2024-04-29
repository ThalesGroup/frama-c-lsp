open Jsonrpc 

(* Handle JSON-RPC request *)
let handle_request request =
  (* Perform some action based on the method_name in the request *)
  Printf.printf "handle_request\n%!";
  match request.method_ with
  | `String "textDocument/definition" ->
      generate_response ~result:(Some (Json.of_string  "Definition result")) request.id
  | `String "textDocument/hover" ->
    generate_response ~result:(Some (Json.of_string "Hover result")) request.id
  | _ ->
      generate_response ~error:(Some (Json.of_string "Method not found")) request.id

