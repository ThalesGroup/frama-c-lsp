let registerCapability json sock = 
let request = 
  Types.RequestMessage.json_of_t (Types.RequestMessage.create
    ~jsonrpc:"2.0"
    ~id:(Int 159263487) (* give proper id *)
    ~method_:"client/registerCapability"
    ~params:json
    ())
in
Utils.send_request sock (Json.save_string request)

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