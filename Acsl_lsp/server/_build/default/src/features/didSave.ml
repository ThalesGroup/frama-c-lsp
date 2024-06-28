let handle (req : Types.NotificationMessage.t) sock : unit = 
  let req_json = (Types.NotificationMessage.json_of_t req) in
  let filename =  Utils.remove_file_scheme (Utils.remove_newline 
    (Utils.remove_quotes 
      (Json.save_string 
        (Json.field "uri" 
          (Json.field "textDocument" 
          (Json.field "params" req_json)))))) in
  
  Load.load_file filename sock;

  