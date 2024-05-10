module Message = struct
  type t = { jsonrpc : float }

  let json_of_t (t : t) : Json.t =
    Json.of_fields ["jsonrpc", Json.of_float t.jsonrpc]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let jsonrpc =
        match List.assoc "jsonrpc" fields with
        | `Float s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format: 'jsonrpc' field must be a float")
      in
      { jsonrpc }
    | _ -> raise (Invalid_argument "Invalid JSON format: expected an object")
end

module RequestMessage = struct
  type id_ = Int of int | Str of string
  type t = {
    jsonrpc : float;
    id : id_;
    method_ : string;
    params : Json.t option (* array or object *)
  }

  let json_of_t (msg : t) : Json.t =
    let id_json =
      match msg.id with
      | Int i -> `Int i
      | Str s -> `String s
    in
    let params_json =
      match msg.params with
      | Some p -> p
      | None -> `Null
    in
    `Assoc [
      "jsonrpc", `Float msg.jsonrpc;
      "id", id_json;
      "method", `String msg.method_;
      "params", params_json;
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let jsonrpc =
        match List.assoc "jsonrpc" fields with
        | `Float s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for RequestMessage: jsonrpc")
      in
      let id =
        match List.assoc "id" fields with
        | `Int i -> Int i
        | `String s -> Str s
        | _ -> raise (Invalid_argument "Invalid JSON format for RequestMessage: id")
      in
      let method_ =
        match List.assoc "method" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for RequestMessage: method")
      in
      let params =
        match List.assoc_opt "params" fields with
        | Some p -> Some p
        | None -> None
      in
      { jsonrpc; id; method_; params }
    | _ -> raise (Invalid_argument "Invalid JSON format for RequestMessage")
end

module NotificationMessage = struct
  type t = {
    jsonrpc : float;
    method_ : string;
    params : Json.t array option
  }

  let json_of_t (msg : t) : Json.t =
    match msg.params with
    | Some params -> `Assoc [("method", `String msg.method_); ("params", `List (Array.to_list params))]
    | None -> `Assoc [("method", `String msg.method_); ("params", `Null)]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let jsonrpc =
        match List.assoc "jsonrpc" fields with
        | `Float s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for NotificationMessage: method")
      in
      let method_ =
        match List.assoc "method" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for NotificationMessage: method")
      in
      let params =
        match List.assoc_opt "params" fields with
        | Some (`List l) -> Some (Array.of_list l)
        | Some `Null -> None
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for NotificationMessage: params")
      in
      { jsonrpc; method_ ; params }
    | _ -> raise (Invalid_argument "Invalid JSON format for NotificationMessage")

end

module ProgressToken = struct
  type t = Int of int | Str of string

  let json_of_t (token : t) : Json.t =
    match token with
    | Int i -> `Int i
    | Str s -> `String s

  let t_of_json (json : Json.t) : t =
    match json with
    | `Int i -> Int i
    | `String s -> Str s
    | _ -> raise (Invalid_argument "Invalid JSON format for ProgressToken")
end

module DocumentUri = struct
  type t = string
  let json_of_t (uri : t) : Json.t =
    Json.of_string uri
  let t_of_json (json : Json.t) : t =
    match json with
    | `String s -> s
    | _ -> raise (Invalid_argument "Invalid JSON format for DocumentUri")
end

module URI = struct
  type t = string
  let json_of_t (uri : t) : Json.t =
    Json.of_string uri
  let t_of_json (json : Json.t) : t =
    match json with
    | `String s -> s
    | _ -> raise (Invalid_argument "Invalid JSON format for DocumentUri")
end

module TraceValue = struct
  type t = string
  let json_of_t (value : t) : Json.t =
    match value with
    | "off" -> `String "off"
    | "messages" -> `String "messages"
    | "verbose" -> `String "verbose"
    | _ -> `Null
  let t_of_json (json : Json.t) : t =
    match json with
    | `String "off" -> "off"
    | `String "messages" -> "messages"
    | `String "verbose" -> "verbose"
    | _ -> raise (Invalid_argument "Invalid JSON format for TraceValue")
end

module WorkspaceFolder = struct
  type t = {
    uri : URI.t;
    name : string
  }
  let json_of_t (folder : t) : Json.t =
  `Assoc [
      "uri", URI.json_of_t folder.uri;
      "name", Json.of_string folder.name
    ]
  let t_of_json (json : Json.t) : t =
  match json with
  | `Assoc fields ->
    let uri_json =
  match List.assoc_opt "uri" fields with
      | Some uri_json -> uri_json
      | _ -> raise (Invalid_argument "Invalid JSON format for WorkspaceFolder: uri")
    in
    let name =
  match List.assoc "name" fields with
      | `String s -> s
      | _ -> raise (Invalid_argument "Invalid JSON format for WorkspaceFolder: name")
    in
    let uri = URI.t_of_json uri_json in
  { uri; name }
  | _ -> raise (Invalid_argument "Invalid JSON format for WorkspaceFolder")

end

module WorkDoneProgressParams = struct
  type t = {
    work_done_token : ProgressToken.t option; 
  }
  let json_of_t (params : t) : Json.t =
    let work_done_token_json =
      match params.work_done_token with
      | Some token -> ProgressToken.json_of_t token
      | None -> `Null
    in
    `Assoc [
      "work_done_token", work_done_token_json
    ]
  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let work_done_token =
        match List.assoc_opt "work_done_token" fields with
        | Some token_json -> Some (ProgressToken.t_of_json token_json)
        | None -> None
      in
      { work_done_token }
    | _ -> raise (Invalid_argument "Invalid JSON format for WorkDoneProgressParams")
end

module DefinitionClientCapabilities = struct
  type t = {
    dynamicRegistration : bool option;
    linkSupport : bool option
  }
  let json_of_t (capabilities : t) : Json.t =
    `Assoc [
      "dynamicRegistration", (match capabilities.dynamicRegistration with Some b -> `Bool b | None -> `Null);
      "linkSupport", (match capabilities.linkSupport with Some b -> `Bool b | None -> `Null)
    ]
  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let dynamicRegistration =
        match List.assoc_opt "dynamicRegistration" fields with
        | Some (`Bool b) -> Some b
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionClientCapabilities: dynamicRegistration")
      in
      let linkSupport =
        match List.assoc_opt "linkSupport" fields with
        | Some (`Bool b) -> Some b
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionClientCapabilities: linkSupport")
      in
      { dynamicRegistration; linkSupport }
    | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionClientCapabilities")
end

module TextDocumentClientCapabilities = struct
  type t = {
    definition : DefinitionClientCapabilities.t option
  }

  let json_of_t (capabilities : t) : Json.t =
    match capabilities.definition with
    | Some def_caps -> Json.of_fields ["definition", DefinitionClientCapabilities.json_of_t def_caps]
    | None -> Json.of_fields []

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let definition =
        match List.assoc_opt "definition" fields with
        | Some def_json -> Some (DefinitionClientCapabilities.t_of_json def_json)
        | None -> None
      in
      { definition }
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentClientCapabilities")
end

module ClientCapabilities = struct
  type t = {
    textDocument : TextDocumentClientCapabilities.t option
  }

  let json_of_t (capabilities : t) : Json.t =
    match capabilities.textDocument with
    | Some text_caps -> Json.of_fields ["textDocument", TextDocumentClientCapabilities.json_of_t text_caps]
    | None -> Json.of_fields []

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let textDocument =
        match List.assoc_opt "textDocument" fields with
        | Some text_json -> Some (TextDocumentClientCapabilities.t_of_json text_json)
        | None -> None
      in
      { textDocument }
    | _ -> raise (Invalid_argument "Invalid JSON format for ClientCapabilities")
end

module InitializeParams = struct
  type client_info = {
    name : string;
    version : string option;
  }

  type t = {
    work_done_token : ProgressToken.t option;
    process_id : int option;
    clientInfo : client_info option;
    locale : string option;
    initialization_options : Json.t option;
    capabilities : ClientCapabilities.t;
    trace : TraceValue.t option;
    workspace_folders : (WorkspaceFolder.t array) option;
  }

  let json_of_client_info (info : client_info) : Json.t =
    `Assoc (List.filter_map (fun (k, v) -> match v with Some v -> Some (k, `String v) | None -> None) [("name", Some info.name); ("version", info.version)])

  let json_of_t (params : t) : Json.t =
    `Assoc ([
      "workDoneToken", (match params.work_done_token with Some token -> ProgressToken.json_of_t token | None -> `Null);
      "process_id", (match params.process_id with Some id -> `Int id | None -> `Null);
      "clientInfo", (match params.clientInfo with Some info -> json_of_client_info info | None -> `Null);
      "locale", (match params.locale with Some loc -> `String loc | None -> `Null);
      "initializationOptions", (match params.initialization_options with Some options -> options | None -> `Null);
      "capabilities", ClientCapabilities.json_of_t params.capabilities;
      "trace", (match params.trace with Some trace -> TraceValue.json_of_t trace | None -> `Null);
    ] @ match params.workspace_folders with
        | Some folders -> ["workspaceFolders", `List (List.map (fun folder -> WorkspaceFolder.json_of_t folder) (Array.to_list folders))]
        | None -> [])
  
  let client_info_of_json (json : Json.t) : client_info =
    match json with
    | `Assoc fields ->
      let name =
        match List.assoc "name" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for client_info: name")
      in
      let version =
        match List.assoc_opt "version" fields with
        | Some (`String s) -> Some s
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for client_info: version")
      in
      { name; version }
    | _ -> raise (Invalid_argument "Invalid JSON format for client_info")

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let work_done_token =
        match List.assoc_opt "workDoneToken" fields with
        | Some `Null -> None
        | Some token_json -> Some (ProgressToken.t_of_json token_json)
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: workDoneToken")
      in
      let process_id =
        match List.assoc_opt "process_id" fields with
        | Some `Null -> None
        | Some (`Int id) -> Some id
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: process_id")
      in
      let clientInfo =
        match List.assoc_opt "clientInfo" fields with
        | Some `Null -> None
        | Some info_json -> Some (client_info_of_json info_json)
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: clientInfo")
      in
      let locale =
        match List.assoc_opt "locale" fields with
        | Some `Null -> None
        | Some (`String loc) -> Some loc
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: locale")
      in
      let initialization_options =
        match List.assoc_opt "initializationOptions" fields with
        | Some `Null -> None
        | Some options -> Some options
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: initializatiOptions")
      in
      let capabilities =
        match List.assoc_opt "capabilities" fields with
        | Some capabilities_json -> ClientCapabilities.t_of_json capabilities_json
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: capabilities")
      in
      let trace =
        match List.assoc_opt "trace" fields with
        | Some `Null -> None
        | Some trace_json -> Some (TraceValue.t_of_json trace_json)
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: trace")
      in
      let workspace_folders =
        match List.assoc_opt "workspaceFolders" fields with
        | Some (`List folder_jsons) -> Some (Array.of_list (List.map (fun folder_json -> WorkspaceFolder.t_of_json folder_json) folder_jsons))
        | Some `Null -> None
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams: workspaceFolders")
      in
      { work_done_token; process_id; clientInfo; locale; initialization_options; capabilities; trace; workspace_folders }
    | _ -> raise (Invalid_argument "Invalid JSON format for InitializeParams")
end

module InitializedParams = struct 
  type t = unit (* No parameters for InitializedParams *)

  let json_of_t (_ : t) : Json.t =
    Json.of_fields []

  let t_of_json (_ : Json.t) : t =
    ()
end

module Registration = struct
  type t = {
    id : string;
    method_ : string;
    registerOptions : Json.t option
  }

  let json_of_t (reg : t) : Json.t =
    let options_json =
      match reg.registerOptions with
      | Some options -> options
      | None -> `Null
    in
    Json.of_fields [
      "id", Json.of_string reg.id;
      "method", Json.of_string reg.method_;
      "registerOptions", options_json
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let id =
        match List.assoc "id" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for id")
      in
      let method_ =
        match List.assoc "method" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for method")
      in
      let registerOptions =
        match List.assoc_opt "registerOptions" fields with
        | Some options -> Some options
        | None -> None
      in
      { id; method_; registerOptions }
    | _ -> raise (Invalid_argument "Invalid JSON format for Registration")
end

module RegistrationParams = struct
  type t = {registrations : Registration.t array}

  let json_of_t (params : t) : Json.t =
    `Assoc [
      "registrations", `List (Array.to_list (Array.map Registration.json_of_t params.registrations))
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let registrations =
        match List.assoc_opt "registrations" fields with
        | Some (`List reg_jsons) -> Array.of_list (List.map Registration.t_of_json reg_jsons)
        | _ -> raise (Invalid_argument "Invalid JSON format for RegistrationParams: registrations")
      in
      { registrations }
    | _ -> raise (Invalid_argument "Invalid JSON format for RegistrationParams")
end

module StaticRegistrationOptions = struct
  type t = {id : string option}

  let json_of_t (options : t) : Json.t =
    match options.id with
    | Some id -> Json.of_fields ["id", Json.of_string id]
    | None -> Json.of_fields ["id", `Null]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let id =
        match List.assoc_opt "id" fields with
        | Some (`String s) -> Some s
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for StaticRegistrationOptions: id")
      in
      { id }
    | _ -> raise (Invalid_argument "Invalid JSON format for StaticRegistrationOptions")
end

module Unregistration = struct
  type t = {
    id : string;
    method_ : string
  }

  let json_of_t (unregistration : t) : Json.t =
    Json.of_fields [
      "id", Json.of_string unregistration.id;
      "method", Json.of_string unregistration.method_
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let id =
        match List.assoc "id" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for Unregistration: id")
      in
      let method_ =
        match List.assoc "method" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for Unregistration: method")
      in
      { id; method_ }
    | _ -> raise (Invalid_argument "Invalid JSON format for Unregistration")
end

module SetTrace = struct
  type t = {value : TraceValue.t}

  let json_of_t (set_trace : t) : Json.t =
    Json.of_fields ["value", TraceValue.json_of_t set_trace.value]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let value =
        match List.assoc_opt "value" fields with
        | Some v -> TraceValue.t_of_json v
        | _ -> raise (Invalid_argument "Invalid JSON format for SetTrace: value")
      in
      { value }
    | _ -> raise (Invalid_argument "Invalid JSON format for SetTrace")
end

module LogTraceParams = struct
  type t = {
    message : string;
    verbose : string option
  }

  let json_of_t (params : t) : Json.t =
    match params.verbose with
    | Some v -> Json.of_fields ["message", Json.of_string params.message; "verbose", Json.of_string v]
    | None -> Json.of_fields ["message", Json.of_string params.message; "verbose", `Null]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let message =
        match List.assoc "message" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for LogTraceParams: message")
      in
      let verbose =
        match List.assoc_opt "verbose" fields with
        | Some (`String s) -> Some s
        | Some `Null -> None
        | None | _ -> raise (Invalid_argument "Invalid JSON format for LogTraceParams: verbose")
      in
      { message; verbose }
    | _ -> raise (Invalid_argument "Invalid JSON format for LogTraceParams")
end

module UnregistrationParams = struct
  type t = { unregistrations : Unregistration.t array }

  let json_of_t (params : t) : Json.t =
    `Assoc ["unregistrations", `List (Array.to_list (Array.map Unregistration.json_of_t params.unregistrations))]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let unregistrations =
        match List.assoc "unregistrations" fields with
        | `List items ->
          Array.of_list (List.map (fun item -> Unregistration.t_of_json item) items)
        | _ -> raise (Invalid_argument "Invalid JSON format for UnregistrationParams: unregistrations")
      in
      { unregistrations }
    | _ -> raise (Invalid_argument "Invalid JSON format for UnregistrationParams")
end

(* to review *)
module TextDocumentSyncKind = struct
  type t = None | Full | Incremental
  let json_of_t (t : t) : Json.t =
    match t with
    | None -> `Int 0
    | Full -> `Int 1
    | Incremental -> `Int 2

  let t_of_json (json : Json.t) : t =
    match json with
    | `Int 0 -> None
    | `Int 1 -> Full
    | `Int 2 -> Incremental
    | _ -> None (* unsure *)
end

module TextDocumentSyncOptions = struct 
  type t = {
    openClose : bool option;
    change : TextDocumentSyncKind.t option
  }
  let json_of_t (options : t) : Json.t =
    `Assoc [
      "openClose", (match options.openClose with Some b -> `Bool b | None -> `Null);
      "change", (match options.change with
                 | Some kind -> TextDocumentSyncKind.json_of_t kind
                 | None -> `Null)
    ]
  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let openClose =
        match List.assoc_opt "openClose" fields with
        | Some (`Bool b) -> Some b
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentSyncOptions: openClose")
      in
      let change =
        match List.assoc_opt "change" fields with
        | Some json -> Some (TextDocumentSyncKind.t_of_json json)
        | None -> None
      in
      { openClose; change }
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentSyncOptions")
end

module TextDocumentItem = struct
  type t = {
    uri : DocumentUri.t;
    languageId : string;
    version : int;
    text : string;
  }
  let json_of_t (item : t) : Json.t =
    `Assoc [
      "uri", `String item.uri;
      "languageId", `String item.languageId;
      "version", `Int item.version;
      "text", `String item.text;
    ]
  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let uri =
        match List.assoc "uri" fields with
        | `String uri_str -> uri_str
        | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentItem: uri")
      in
      let languageId =
        match List.assoc "languageId" fields with
        | `String lang_id -> lang_id
        | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentItem: languageId")
      in
      let version =
        match List.assoc "version" fields with
        | `Int ver -> ver
        | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentItem: version")
      in
      let text =
        match List.assoc "text" fields with
        | `String txt -> txt
        | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentItem: text")
      in
      { uri; languageId; version; text }
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentItem")
end

module DidOpenTextDocumentParams = struct
  type t = {
    textDocument : TextDocumentItem.t
  }

  let json_of_t (params : t) : Json.t =
    `Assoc [
      "textDocument", TextDocumentItem.json_of_t params.textDocument
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let textDocument =
        match List.assoc "textDocument" fields with
        | json -> TextDocumentItem.t_of_json json
      in
      { textDocument }
    | _ -> raise (Invalid_argument "Invalid JSON format for DidOpenTextDocumentParams")
end

module TextDocumentChangeRegistrationOptions = struct
  type t = { syncKind : TextDocumentSyncKind.t }
  let json_of_t (options : t) : Json.t =
    `Assoc [
      "syncKind", TextDocumentSyncKind.json_of_t options.syncKind
    ]
  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let syncKind =
        match List.assoc_opt "syncKind" fields with
        | Some syncKindJson -> TextDocumentSyncKind.t_of_json syncKindJson
        | None -> raise (Invalid_argument "Invalid JSON format for TextDocumentChangeRegistrationOptions: syncKind")
      in
      { syncKind }
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentChangeRegistrationOptions")
end

module VersionedTextDocumentIdentifier = struct
  type t = {
    uri : DocumentUri.t;
    version : int
  }

  let json_of_t (identifier : t) : Json.t =
    `Assoc [
      "uri", `String identifier.uri;
      "version", `Int identifier.version
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let uri =
        match List.assoc_opt "uri" fields with
        | Some (`String uri) -> uri
        | _ -> raise (Invalid_argument "Invalid JSON format for VersionedTextDocumentIdentifier: uri")
      in
      let version =
        match List.assoc_opt "version" fields with
        | Some (`Int version) -> version
        | _ -> raise (Invalid_argument "Invalid JSON format for VersionedTextDocumentIdentifier: version")
      in
      { uri; version }
    | _ -> raise (Invalid_argument "Invalid JSON format for VersionedTextDocumentIdentifier")
end

module DefinitionOptions = struct
  type t = { workDoneProgress : bool option }

  let json_of_t (options : t) : Json.t =
    `Assoc [
      "workDoneProgress", (match options.workDoneProgress with Some b -> `Bool b | None -> `Null)
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let workDoneProgress =
        match List.assoc_opt "workDoneProgress" fields with
        | Some (`Bool b) -> Some b
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionOptions: workDoneProgress")
      in
      { workDoneProgress }
    | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionOptions")
end


module ServerCapabilities = struct
  type definition_provider = Bool of bool | DefinitionOptions of DefinitionOptions.t
  type t = {
    definitionProvider : definition_provider option
  }

  let json_of_definition_provider (provider : definition_provider) : Json.t =
    match provider with
    | Bool b -> `Bool b
    | DefinitionOptions options -> DefinitionOptions.json_of_t options

  let definition_provider_of_json (json : Json.t) : definition_provider =
    match json with
    | `Bool b -> Bool b
    | _ -> DefinitionOptions (DefinitionOptions.t_of_json json)

  let json_of_t (capabilities : t) : Json.t =
    match capabilities.definitionProvider with
    | Some provider -> json_of_definition_provider provider
    | None -> `Null

  let t_of_json (json : Json.t) : t =
    match json with
    | `Null -> { definitionProvider = None }
    | _ -> { definitionProvider = Some (definition_provider_of_json json) }
end

module InitializeResult = struct
  type server_info = {
    name : string;
    version : string option;
  }
  type t = {
    capabilities : ServerCapabilities.t option;
    serverInfo : server_info option
  }

  let json_of_server_info (info : server_info) : Json.t =
    match info.version with
    | Some version -> `Assoc ["name", `String info.name; "version", `String version]
    | None -> `Assoc ["name", `String info.name]

  let server_info_of_json (json : Json.t) : server_info =
    match json with
    | `Assoc fields ->
      let name =
        match List.assoc "name" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for server_info: name")
      in
      let version =
        match List.assoc_opt "version" fields with
        | Some (`String v) -> Some v
        | _ -> None
      in
      { name; version }
    | _ -> raise (Invalid_argument "Invalid JSON format for server_info")

  let json_of_t (result : t) : Json.t =
    match result.serverInfo with
    | Some info -> json_of_server_info info
    | None -> `Assoc []

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let capabilities =
        match List.assoc_opt "capabilities" fields with
        | Some (`Assoc caps_json) -> Some (ServerCapabilities.t_of_json (`Assoc caps_json))
        | _ -> None
      in
      let serverInfo =
        match List.assoc_opt "serverInfo" fields with
        | Some info_json -> Some (server_info_of_json info_json)
        | _ -> None
      in
      { capabilities; serverInfo }
    | _ -> raise (Invalid_argument "Invalid JSON format for InitializeResult")
end

module DocumentFilter = struct
  type t = {
    language : string option;
    scheme : string option;
    pattern : string
  }

  let json_of_t (filter : t) : Json.t =
    `Assoc (List.filter_map (fun (k, v) -> Option.map (fun v' -> (k, `String v')) v) [
      "language", filter.language;
      "scheme", filter.scheme;
      "pattern", Some filter.pattern
    ])

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let language =
        match List.assoc_opt "language" fields with
        | Some (`String lang) -> Some lang
        | _ -> None
      in
      let scheme =
        match List.assoc_opt "scheme" fields with
        | Some (`String sch) -> Some sch
        | _ -> None
      in
      let pattern =
        match List.assoc "pattern" fields with
        | `String pat -> pat
        | _ -> raise (Invalid_argument "Invalid JSON format for DocumentFilter: pattern")
      in
      { language; scheme; pattern }
    | _ -> raise (Invalid_argument "Invalid JSON format for DocumentFilter")
end

module DocumentSelector = struct
  type t = DocumentFilter.t array

  let json_of_t (selector : t) : Json.t =
    `List (Array.to_list (Array.map DocumentFilter.json_of_t selector))

  let t_of_json (json : Json.t) : t =
    match json with
    | `List items ->
      Array.of_list (List.map (fun item -> DocumentFilter.t_of_json item) items)
    | _ -> raise (Invalid_argument "Invalid JSON format for DocumentSelector")
end

module WorkDoneProgressOptions = struct
  type t = {
    workDoneProgress : bool option
  }

  let json_of_t (options : t) : Json.t =
    `Assoc [
      "workDoneProgress", (match options.workDoneProgress with Some b -> `Bool b | None -> `Null)
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let workDoneProgress =
        match List.assoc_opt "workDoneProgress" fields with
        | Some (`Bool b) -> Some b
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for WorkDoneProgressOptions: workDoneProgress")
      in
      { workDoneProgress }
    | _ -> raise (Invalid_argument "Invalid JSON format for WorkDoneProgressOptions")
end

module TextDocumentRegistrationOptions = struct
  type document_selector = DocumentSelector of DocumentSelector.t | Null
  type t = {
    documentSelector : document_selector
  }

  let json_of_document_selector (selector : document_selector) : Json.t =
    match selector with
    | DocumentSelector doc_sel -> DocumentSelector.json_of_t doc_sel
    | Null -> `Null

  let document_selector_of_json (json : Json.t) : document_selector =
    match json with
    | `Null -> Null
    | _ -> DocumentSelector (DocumentSelector.t_of_json json)

  let json_of_t (options : t) : Json.t =
    `Assoc [
      "documentSelector", json_of_document_selector options.documentSelector
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let documentSelector =
        match List.assoc_opt "documentSelector" fields with
        | Some doc_sel_json -> document_selector_of_json doc_sel_json
        | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentRegistrationOptions: documentSelector")
      in
      { documentSelector }
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentRegistrationOptions")
end

module DefinitionRegistrationOptions = struct
  type document_selector = DocumentSelector of DocumentSelector.t | Null
  type t = {
    documentSelector : document_selector;
    definitionOptions : DefinitionOptions.t;
  }

  let json_of_document_selector = function
    | DocumentSelector doc_sel -> DocumentSelector.json_of_t doc_sel
    | Null -> `Null

  let json_of_t (options : t) : Json.t =
    let doc_selector_json = json_of_document_selector options.documentSelector in
    let def_options_json = DefinitionOptions.json_of_t options.definitionOptions in
    `Assoc [
      "documentSelector", doc_selector_json;
      "definitionOptions", def_options_json
    ]

  let document_selector_of_json json =
    match json with
    | `Null -> Null
    | _ -> DocumentSelector (DocumentSelector.t_of_json json)

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let documentSelector =
        match List.assoc_opt "documentSelector" fields with
        | Some doc_sel_json -> document_selector_of_json doc_sel_json
        | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionRegistrationOptions: documentSelector")
      in
      let definitionOptions =
        match List.assoc_opt "definitionOptions" fields with
        | Some def_opt_json -> DefinitionOptions.t_of_json def_opt_json
        | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionRegistrationOptions: definitionOptions")
      in
      { documentSelector; definitionOptions }
    | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionRegistrationOptions")
end

module TextDocumentIdentifier = struct
  type t = { uri : DocumentUri.t }

  let json_of_t (identifier : t) : Json.t =
    `Assoc ["uri", DocumentUri.json_of_t identifier.uri]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let uri =
        match List.assoc "uri" fields with
        | uri_json -> DocumentUri.t_of_json uri_json
      in
      { uri }
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentIdentifier")
end

module Position = struct
  type t = {
    line : int; (* unsigned *)
    character : int (* unsigned *)
  }

  let json_of_t (position : t) : Json.t =
    `Assoc [
      "line", `Int position.line;
      "character", `Int position.character
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let line =
        match List.assoc "line" fields with
        | `Int i -> i
        | _ -> raise (Invalid_argument "Invalid JSON format for Position: line")
      in
      let character =
        match List.assoc "character" fields with
        | `Int i -> i
        | _ -> raise (Invalid_argument "Invalid JSON format for Position: character")
      in
      { line; character }
    | _ -> raise (Invalid_argument "Invalid JSON format for Position")
end

module Range = struct
  type t = {
    start : Position.t;
    end_ : Position.t
  }

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let start =
        match List.assoc_opt "start" fields with
        | Some start_json -> Position.t_of_json start_json
        | None -> raise (Invalid_argument "Invalid JSON format for Range: start")
      in
      let end_ =
        match List.assoc_opt "end" fields with
        | Some end_json -> Position.t_of_json end_json
        | None -> raise (Invalid_argument "Invalid JSON format for Range: end")
      in
      { start; end_ }
    | _ -> raise (Invalid_argument "Invalid JSON format for Range")
  
  let json_of_t (range : t) : Json.t =
    `Assoc [
      "start", Position.json_of_t range.start;
      "end", Position.json_of_t range.end_;
    ]
end

module TextDocumentContentChangeEvent = struct
  type t =
  | RangeChange of { range: Range.t; rangeLength: int option; text: string }
  | FullTextChange of { text: string }

  let json_of_t (change : t) : Json.t =
    match change with
    | RangeChange { range; rangeLength; text } ->
      `Assoc [
        "range", Range.json_of_t range;
        "rangeLength", (match rangeLength with Some len -> `Int len | None -> `Null);
        "text", `String text
      ]
    | FullTextChange { text } ->
      `Assoc [
        "text", `String text
      ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      if List.mem_assoc "range" fields && List.mem_assoc "text" fields then
        let range = Range.t_of_json (List.assoc "range" fields) in
        let rangeLength =
          match List.assoc_opt "rangeLength" fields with
          | Some (`Int len) -> Some len
          | Some `Null -> None
          | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentContentChangeEvent: rangeLength")
        in
        let text =
          match List.assoc "text" fields with
          | `String s -> s
          | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentContentChangeEvent: text")
        in
        RangeChange { range; rangeLength; text }
      else if List.mem_assoc "text" fields then
        let text =
          match List.assoc "text" fields with
          | `String s -> s
          | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentContentChangeEvent: text")
        in
        FullTextChange { text }
      else
        raise (Invalid_argument "Invalid JSON format for TextDocumentContentChangeEvent")
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentContentChangeEvent")
end


module DidChangeTextDocumentParams = struct
  type t = {
    textDocument : VersionedTextDocumentIdentifier.t;
    contentChanges : TextDocumentContentChangeEvent.t array;
  }

  let json_of_t (params : t) : Json.t =
    `Assoc [
      "textDocument", VersionedTextDocumentIdentifier.json_of_t params.textDocument;
      "contentChanges", `List (Array.to_list (Array.map TextDocumentContentChangeEvent.json_of_t params.contentChanges))
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let textDocument =
        match List.assoc "textDocument" fields with
        | doc_json -> VersionedTextDocumentIdentifier.t_of_json doc_json
      in
      let contentChanges =
        match List.assoc "contentChanges" fields with
        | `List change_list ->
          Array.of_list (List.map (fun json -> TextDocumentContentChangeEvent.t_of_json json) change_list)
        | _ -> raise (Invalid_argument "Invalid JSON format for DidChangeTextDocumentParams: contentChanges")
      in
      { textDocument; contentChanges }
    | _ -> raise (Invalid_argument "Invalid JSON format for DidChangeTextDocumentParams")
end

module DidCloseTextDocumentParams = struct
  type t = { textDocument : TextDocumentIdentifier.t }

  let json_of_t (params : t) : Json.t =
    `Assoc [
      "textDocument", TextDocumentIdentifier.json_of_t params.textDocument
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let textDocument =
        match List.assoc "textDocument" fields with
        | doc_json -> TextDocumentIdentifier.t_of_json doc_json
      in
      { textDocument }
    | _ -> raise (Invalid_argument "Invalid JSON format for DidCloseTextDocumentParams")
end



module TextDocumentPositionParams = struct
  type t = {
    textDocument : TextDocumentIdentifier.t;
    position : Position.t
  }

  let json_of_t (params : t) : Json.t =
    `Assoc [
      "textDocument", TextDocumentIdentifier.json_of_t params.textDocument;
      "position", Position.json_of_t params.position
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let textDocument =
        match List.assoc "textDocument" fields with
        | json -> TextDocumentIdentifier.t_of_json json
      in
      let position =
        match List.assoc "position" fields with
        | json -> Position.t_of_json json
      in
      { textDocument; position }
    | _ -> raise (Invalid_argument "Invalid JSON format for TextDocumentPositionParams")
end

module PartialResultParams = struct
  type t = { partialResultToken : ProgressToken.t option }

  let json_of_t (params : t) : Json.t =
    `Assoc [
      "partialResultToken", (match params.partialResultToken with Some t -> ProgressToken.json_of_t t | None -> `Null)
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let partialResultToken =
        match List.assoc_opt "partialResultToken" fields with
        | Some token_json -> Some (ProgressToken.t_of_json token_json)
        | None -> None
      in
      { partialResultToken }
    | _ -> raise (Invalid_argument "Invalid JSON format for PartialResultParams")
end

module DefinitionParams = struct
  type t = {
    partialResultToken : ProgressToken.t option;
    textDocument : TextDocumentIdentifier.t;
    position : Position.t;
    work_done_token : ProgressToken.t option
  }

  let json_of_t (params : t) : Json.t =
    let partial_result_token_json =
      match params.partialResultToken with
      | Some token -> ProgressToken.json_of_t token
      | None -> `Null
    in
    let text_document_json = TextDocumentIdentifier.json_of_t params.textDocument in
    let position_json = Position.json_of_t params.position in
    let work_done_token_json =
      match params.work_done_token with
      | Some token -> ProgressToken.json_of_t token
      | None -> `Null
    in
    `Assoc [
      "partialResultToken", partial_result_token_json;
      "textDocument", text_document_json;
      "position", position_json;
      "work_done_token", work_done_token_json
    ]

    let t_of_json (json : Json.t) : t =
      match json with
      | `Assoc fields ->
        let partialResultToken =
          match List.assoc_opt "partialResultToken" fields with
          | Some token_json -> Some (ProgressToken.t_of_json token_json)
          | None -> None
        in
        let textDocument =
          match List.assoc_opt "textDocument" fields with
          | Some text_doc_json -> TextDocumentIdentifier.t_of_json text_doc_json
          | None -> raise (Invalid_argument "Invalid JSON format for DefinitionParams: textDocument")
        in
        let position =
          match List.assoc_opt "position" fields with
          | Some position_json -> Position.t_of_json position_json
          | None -> raise (Invalid_argument "Invalid JSON format for DefinitionParams: position")
        in
        let work_done_token =
          match List.assoc_opt "work_done_token" fields with
          | Some token_json -> Some (ProgressToken.t_of_json token_json)
          | None -> None
        in
        { partialResultToken; textDocument; position; work_done_token }
      | _ -> raise (Invalid_argument "Invalid JSON format for DefinitionParams")
    
end

module Location = struct 
  type t = {
    uri : DocumentUri.t;
    range : Range.t
  }

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let uri =
        match List.assoc_opt "uri" fields with
        | Some uri_json -> DocumentUri.t_of_json uri_json
        | None -> raise (Invalid_argument "Invalid JSON format for Location: uri")
      in
      let range =
        match List.assoc_opt "range" fields with
        | Some range_json -> Range.t_of_json range_json
        | None -> raise (Invalid_argument "Invalid JSON format for Location: range")
      in
      { uri; range }
    | _ -> raise (Invalid_argument "Invalid JSON format for Location: expected an object")

    let json_of_t (loc : t) : Json.t =
      `Assoc [
        "uri", DocumentUri.json_of_t loc.uri;
        "range", Range.json_of_t loc.range;
      ]
end

module DiagnosticSeverity = struct
  type t = Error | Warning | Information | Hint

  let json_of_t (severity : t) : Json.t =
    match severity with
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let t_of_json (json : Json.t) : t =
    match json with
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | _ -> raise (Invalid_argument "Invalid JSON format for DiagnosticSeverity")
end

module CodeDescription = struct 
  type t = {
    href : URI.t
  }

  let json_of_t (code_desc : t) : Json.t =
    `Assoc [
      "href", URI.json_of_t code_desc.href
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let href =
        match List.assoc_opt "href" fields with
        | Some json_uri -> URI.t_of_json json_uri
        | _ -> raise (Invalid_argument "Invalid JSON format for CodeDescription: href")
      in
      { href }
    | _ -> raise (Invalid_argument "Invalid JSON format for CodeDescription")
end

module DiagnosticTag = struct
  type t = Unnecessary | Deprecated

  let json_of_t (tag : t) : Json.t =
    match tag with
    | Unnecessary -> `String "unnecessary"
    | Deprecated -> `String "deprecated"

  let t_of_json (json : Json.t) : t =
    match json with
    | `String "unnecessary" -> Unnecessary
    | `String "deprecated" -> Deprecated
    | _ -> raise (Invalid_argument "Invalid JSON format for DiagnosticTag")
end

module DiagnosticRelatedInformation = struct
  type t = {
    location : Location.t;
    message : string
  }
  let json_of_t (info : t) : Json.t =
    `Assoc [
      "location", Location.json_of_t info.location;
      "message", Json.of_string info.message
    ]
  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let location =
        match List.assoc "location" fields with
        | loc_json -> Location.t_of_json loc_json
      in
      let message =
        match List.assoc "message" fields with
        | `String msg -> msg
        | _ -> raise (Invalid_argument "Invalid JSON format for DiagnosticRelatedInformation: message")
      in
      { location; message }
    | _ -> raise (Invalid_argument "Invalid JSON format for DiagnosticRelatedInformation")
end

module Diagnostic = struct
  type code_ = Int of int | Str of string
  type t = {
    range : Range.t;
    severity : DiagnosticSeverity.t option;
    code : code_ option ;
    codeDescription: CodeDescription.t option;
    source: string option;
    message: string;
    tags: DiagnosticTag.t array option;
    relatedInformation: DiagnosticRelatedInformation.t array option;
    data: Json.t option;
  }

  let json_of_t (diag : t) : Json.t =
    `Assoc [
      "range", Range.json_of_t diag.range;
      "severity", (match diag.severity with Some sev -> DiagnosticSeverity.json_of_t sev | None -> `Null);
      "code", (match diag.code with Some (Int i) -> `Int i | Some (Str s) -> `String s | None -> `Null);
      "codeDescription", (match diag.codeDescription with Some desc -> CodeDescription.json_of_t desc | None -> `Null);
      "source", (match diag.source with Some src -> `String src | None -> `Null);
      "message", `String diag.message;
      "tags", (match diag.tags with Some tags -> `List (Array.to_list (Array.map DiagnosticTag.json_of_t tags)) | None -> `Null);
      "relatedInformation", (match diag.relatedInformation with Some infos -> `List (Array.to_list (Array.map DiagnosticRelatedInformation.json_of_t infos)) | None -> `Null);
      "data", (match diag.data with Some d -> d | None -> `Null);
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let range = Range.t_of_json (List.assoc "range" fields) in
      let severity =
        match List.assoc_opt "severity" fields with
        | Some json -> Some (DiagnosticSeverity.t_of_json json)
        | None -> None
      in
      let code =
        match List.assoc_opt "code" fields with
        | Some (`Int i) -> Some (Int i)
        | Some (`String s) -> Some (Str s)
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for Diagnostic: code")
      in
      let codeDescription =
        match List.assoc_opt "codeDescription" fields with
        | Some json -> Some (CodeDescription.t_of_json json)
        | None -> None
      in
      let source =
        match List.assoc_opt "source" fields with
        | Some (`String s) -> Some s
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for Diagnostic: source")
      in
      let message =
        match List.assoc "message" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for Diagnostic: message")
      in
      let tags =
        match List.assoc_opt "tags" fields with
        | Some (`List tags_json) -> Some (Array.of_list (List.map DiagnosticTag.t_of_json tags_json))
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for Diagnostic: tags")
      in
      let relatedInformation =
        match List.assoc_opt "relatedInformation" fields with
        | Some (`List infos_json) -> Some (Array.of_list (List.map DiagnosticRelatedInformation.t_of_json infos_json))
        | None -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for Diagnostic: relatedInformation")
      in
      let data =
        match List.assoc_opt "data" fields with
        | Some json -> Some json
        | None -> None
      in
      { range; severity; code; codeDescription; source; message; tags; relatedInformation; data }
    | _ -> raise (Invalid_argument "Invalid JSON format for Diagnostic")
end

module Command = struct
  type t = {
    title : string;
    command : string;
    arguments : Json.t list option
  }

  let json_of_t (cmd : t) : Json.t =
    `Assoc [
      "title", `String cmd.title;
      "command", `String cmd.command;
      "arguments", (match cmd.arguments with Some args -> `List args | None -> `Null)
    ]

  let t_of_json (json : Json.t) : t =
    match json with
    | `Assoc fields ->
      let title =
        match List.assoc "title" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for Command: title")
      in
      let command =
        match List.assoc "command" fields with
        | `String s -> s
        | _ -> raise (Invalid_argument "Invalid JSON format for Command: command")
      in
      let arguments =
        match List.assoc_opt "arguments" fields with
        | Some (`List args) -> Some args
        | Some `Null -> None
        | _ -> raise (Invalid_argument "Invalid JSON format for Command: arguments")
      in
      { title; command; arguments }
    | _ -> raise (Invalid_argument "Invalid JSON format for Command")
end

