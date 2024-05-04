module ProgressToken = struct
  type t = Int of int | Str of string
end

module DocumentUri = struct
  type t = string
end

module URI = struct
  type t = string
end

module rec LSPObject : sig
  type t = (string * LSPAny.t) list
end = struct
  type t = (string * LSPAny.t) list
end

and LSPArray : sig
  type t = LSPAny.t list
end = struct
  type t = LSPAny.t list
end

and LSPAny : sig
  type t =
    | LSPObject of LSPObject.t
    | LSPArray of LSPArray.t
    | String of string
    | Integer of int
    | UInteger of int
    | Decimal of float
    | Boolean of bool
    | Null
end = struct
  type t =
    | LSPObject of LSPObject.t
    | LSPArray of LSPArray.t
    | String of string
    | Integer of int
    | UInteger of int
    | Decimal of float
    | Boolean of bool
    | Null
end

module TraceValue = struct
  type t = Off | Messages | Verbose
end

module WorkspaceFolder = struct
  type t = {
    uri : URI.t;
    name : string
  }
end

module WorkDoneProgressParams = struct
  type progress_token = ProgressToken.t  (* Define the ProgressToken type *)
  type t = {
    work_done_token : progress_token option;  (* Optional token for reporting work done progress *)
  }
end

module DefinitionClientCapabilities = struct
  type t = {
    dynamicRegistration : bool option;
    linkSupport : bool option
  }
end

module TextDocumentClientCapabilities = struct
  type t = {
    definition : DefinitionClientCapabilities.t option
  }
end

module ClientCapabilities = struct
  type t = {
    textDocument : TextDocumentClientCapabilities.t option
  }
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
    root_path : string option;
    root_uri : DocumentUri.t option;
    initialization_options : LSPAny.t option;
    capabilities : ClientCapabilities.t;
    trace : TraceValue.t option;
    workspace_folders : WorkspaceFolder.t list option;
  }
end

module DefinitionOptions = struct
  include WorkDoneProgressParams 
end

module ServerCapabilities = struct
  type definition_provider = Bool of bool | DefinitionOptions of DefinitionOptions.t
  type t = {
    definitionProvider : definition_provider option
  }
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
end

module DocumentFilter = struct
  type t = {
    language : string option;
    scheme : string option;
    pattern: string
  }
end

module DocumentSelector = struct 
  type t = DocumentFilter.t array
end

module WorkDoneProgressOptions = struct
  type t = {
    workDoneProgress : bool option
  }
end

module TextDocumentRegistrationOptions = struct
  type document_selector = DocumentSelector of DocumentSelector.t | Null
  type t = {
    documentSelector : document_selector
  }
end

module DefinitionRegistrationOptions = struct
  include TextDocumentRegistrationOptions
  include DefinitionOptions
end

