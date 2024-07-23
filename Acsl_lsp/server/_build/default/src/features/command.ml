(* request execution of vs code command *)
let displayCIL id = 
  try 
    let ast = (Ast.get ()) in
    let pretty_ast = (Pretty_utils.to_string (Printer.pp_file) ast) in
    (* let escaped_pretty_ast = String.escaped pretty_ast in *)
    let result = Json.of_string (String.escaped pretty_ast) in
    let response = Types.ResponseMessage.json_of_t (Types.ResponseMessage.create 
      ~jsonrpc:"2.0"
      ~id:id (* todo : give proper id *)
      ~result:result
      ()
    )
      in
    response;
  with Ast.Bad_Initialization msg ->
    Types.ResponseError.json_of_t (Types.ResponseError.create
      ~code:(-32899)
      ~message:msg 
      ()
    )

