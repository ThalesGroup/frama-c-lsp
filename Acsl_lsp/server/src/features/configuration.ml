let configs = ref []
let gotConfiguration = ref false
let request_includePaths id client_sock : unit = 
  Printf.printf "Asking for configurations\n%!";
  let req = Types.RequestMessage.json_of_t 
  (Types.RequestMessage.create 
    ~jsonrpc:"2.0" 
    ~id:(Types.Int id) 
    ~method_:"workspace/configuration" 
    ~params:(Json.load_string 
    {|
      {
        "items":
        [
          {"section": "vscodeacsl.includePaths"}
        ]
      }
    |})
    ()) in
  Utils.send_request client_sock (Json.save_string req)

let set_includePaths confs : unit = 
  let formatted_paths = 
    List.map (fun config -> 
      ("-I"^(Utils.remove_newline (Utils.remove_quotes config)))
    ) confs
  in
  Kernel.CppExtraArgs.set (formatted_paths)

let save_config (result: Json.json) = 
  match result with
  | `List [`List s] -> 
    gotConfiguration := true;
    configs := List.map(fun c ->
      Json.save_string(c)
    ) s;
  | _ -> failwith "Invalid include paths"

