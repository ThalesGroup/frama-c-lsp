(* let displayCIL id : Json.json = 
  try 
    let ast = (Ast.get ()) in
    let pretty_ast = (Pretty_utils.to_string (Printer.pp_file) ast) in
    let result = Json.of_string (pretty_ast) in
    let response = Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create 
      ~jsonrpc:"2.0"
      ~id:(Lsp_types.Int id)
      ~result:result
      ()
    )
    in
    response;
  with Ast.Bad_Initialization msg ->
    Lsp_types.ResponseError.json_of_t (Lsp_types.ResponseError.create
      ~code:(-32899)
      ~message:msg 
      ()
    )

*)