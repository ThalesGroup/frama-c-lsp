let registerCapabilityRequest json = 
  Types.RequestMessage.json_of_t (Types.RequestMessage.create
    ~jsonrpc:"2.0"
    ~id:(Int 159263487) (* give proper id *)
    ~method_:"client/registerCapability"
    ~params:json
    ())

let registration method_ = 
    Types.Registration.create
      ~id:"registration"
      ~method_:method_
      ()

let registrationParams registrations = 
  Types.RegistrationParams.json_of_t (
    Types.RegistrationParams.create
      ~registrations:registrations
      ()
  )