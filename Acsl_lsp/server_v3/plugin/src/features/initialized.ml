let init : Json.json = 
  (* Project.set_current (Project.create "default"); *)



  RegisterCapability.registerCapabilityRequest 
  (RegisterCapability.registrationParams 
    ([RegisterCapability.registration "workspace/didChangeConfiguration"])
  );


