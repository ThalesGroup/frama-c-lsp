let shutdown (req : Types.RequestMessage.t) : Json.json = 
  Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())

let shutdown_error (req : Types.RequestMessage.t) : Json.json = 
  Types.ResponseMessage.json_of_t (
    (Types.ResponseMessage.create 
    ~jsonrpc:"2.0" 
    ~id:req.id 
    ~error:(Types.ResponseError.create 
      ~code:(-32600)
      ~message:"Invalid request received after shutdown"
    ()) 
    ) 
  ())
