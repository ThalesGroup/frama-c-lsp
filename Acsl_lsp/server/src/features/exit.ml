let exit : Json.json = 
    let result = "{
      \"jsonrpc\": \"2.0\",
      \"method\": \"exit\",
      \"params\": {}
    }
    " in
    Json.load_string result;

