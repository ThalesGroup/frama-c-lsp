module ProgressToken : sig
  type t = Int of int | Str of string
end

module DocumentUri : sig
  type t = string
end

module URI : sig
  type t = string
end

module rec LSPObject : sig
  type t = (string * LSPAny.t) list
end

and LSPArray : sig
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
end

module TraceValue : sig
  type t = Off | Messages | Verbose
end

module WorkspaceFolder : sig
  type t = {
    uri : URI.t;
    name : string
  }
end

module WorkDoneProgressParams : sig
  type progress_token = ProgressToken.t  (* Define the ProgressToken type *)
  type t = {
    work_done_token : progress_token option;  (* Optional token for reporting work done progress *)
  }
end

module DefinitionClientCapabilities : sig
  type t = {
    dynamicRegistration : bool option;
    linkSupport : bool option
  }
end

module TextDocumentClientCapabilities : sig
  type t = {
    definition : DefinitionClientCapabilities.t option
  }
end

module ClientCapabilities : sig
  type t = {
    textDocument : TextDocumentClientCapabilities.t option
  }
end

module InitializeParams : sig
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

module DefinitionOptions : sig
  type t
end

module ServerCapabilities : sig
  type definition_provider = Bool of bool | DefinitionOptions of DefinitionOptions.t
  type t = {
    definitionProvider : definition_provider option
  }
end

module InitializeResult : sig 
  type server_info = {
    name : string;
    version : string option;
  }
  type t = {
    capabilities : ServerCapabilities.t option;
    serverInfo : server_info option
  }
end

module DocumentFilter : sig
  type t = {
    language : string option;
    scheme : string option;
    pattern: string
  }
end

module DocumentSelector : sig 
  type t = DocumentFilter.t array
end

module WorkDoneProgressOptions : sig
  type t = {
    workDoneProgress : bool option
  }
end

module TextDocumentRegistrationOptions : sig
  type document_selector = DocumentSelector of DocumentSelector.t | Null
  type t = {
    documentSelector : document_selector
  }
end

module DefinitionRegistrationOptions : sig
end

