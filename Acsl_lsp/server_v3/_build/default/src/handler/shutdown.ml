let shutdown (req : Lsp_types.RequestMessage.t) : Json.json = 
  Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())

let shutdown_error (req : Lsp_types.RequestMessage.t) : Json.json = 
  Lsp_types.ResponseMessage.json_of_t (
    (Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0" 
    ~id:req.id 
    ~error:(Lsp_types.ResponseError.create 
      ~code:(-32600)
      ~message:"Invalid request received after shutdown"
    ()) 
    ) 
  ())
