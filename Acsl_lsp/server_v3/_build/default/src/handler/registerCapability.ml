let registerCapabilityRequest json = 
  Lsp_types.RequestMessage.json_of_t (Lsp_types.RequestMessage.create
    ~jsonrpc:"2.0"
    ~id:(Lsp_types.Str "register_capability") (* give proper id *)
    ~method_:"client/registerCapability"
    ~params:json
    ())

let registration method_ = 
    Lsp_types.Registration.create
      ~id:"registration"
      ~method_:method_
      ()

let registrationParams registrations = 
  Lsp_types.RegistrationParams.json_of_t (
    Lsp_types.RegistrationParams.create
      ~registrations:registrations
      ()
  )