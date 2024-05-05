module Message : sig
  type t = {jsonrpc : string}
  include Jsonable.B with type t := t

end

module RequestMessage : sig
  type id_ = Int of int | Str of string
  type t = {
    id : id_;
    method_ : string;
    params : Json.t option (* array or object *)
  }
  include Jsonable.B with type t := t

end

module ProgressToken : sig
  type t = Int of int | Str of string
  include Jsonable.B with type t := t

end

module DocumentUri : sig
  type t = string
  include Jsonable.B with type t := t

end

module URI : sig
  type t = string
  include Jsonable.B with type t := t

end

module TraceValue : sig
  type t = Off | Messages | Verbose
  include Jsonable.B with type t := t

end

module WorkspaceFolder : sig
  type t = {
    uri : URI.t;
    name : string
  }
  include Jsonable.B with type t := t

end

module WorkDoneProgressParams : sig
  type t = {
    work_done_token : ProgressToken.t option;  (* Optional token for reporting work done progress *)
  }
  include Jsonable.B with type t := t

end

module DefinitionClientCapabilities : sig
  type t = {
    dynamicRegistration : bool option;
    linkSupport : bool option
  }
  include Jsonable.B with type t := t

end

module TextDocumentClientCapabilities : sig
  type t = {
    definition : DefinitionClientCapabilities.t option
  }
  include Jsonable.B with type t := t

end

module ClientCapabilities : sig
  type t = {
    textDocument : TextDocumentClientCapabilities.t option
  }
  include Jsonable.B with type t := t

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
    initialization_options : Json.t option;
    capabilities : ClientCapabilities.t;
    trace : TraceValue.t option;
    workspace_folders : WorkspaceFolder.t list option;
  }
  include Jsonable.B with type t := t

end

module InitializedParams : sig 
  type t
  include Jsonable.B with type t := t
end

module Registration : sig
  type t = {
    id : string;
    method_ : string;
    registerOptions : Json.t option
  }
  include Jsonable.B with type t := t

end

module RegistrationParams : sig
  type t = {registrations : Registration.t list}
  include Jsonable.B with type t := t

end
 
module StaticRegistrationOptions : sig 
  type t = {id : string option}
  include Jsonable.B with type t := t

end

module Unregistration : sig
  type t = {
    id : string;
    method_ : string
  }
  include Jsonable.B with type t := t

end

module SetTrace : sig
  type t = {value : TraceValue.t}
  include Jsonable.B with type t := t

end

module LogTraceParams : sig
  type t = {
    message : string;
    verbose : string option
  }
  include Jsonable.B with type t := t

end

module UnegistrationParams : sig
  type t = {unregistrations : Unregistration.t list}
  include Jsonable.B with type t := t

end

module TextDocumentSyncKind : sig
  type t = None | Full | Incremental

  include Jsonable.B with type t := t

end

module DefinitionOptions : sig
  type t
  include Jsonable.B with type t := t

end

module ServerCapabilities : sig
  type definition_provider = Bool of bool | DefinitionOptions of DefinitionOptions.t
  type t = {
    definitionProvider : definition_provider option
  }
  include Jsonable.B with type t := t

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
  include Jsonable.B with type t := t

end

module DocumentFilter : sig
  type t = {
    language : string option;
    scheme : string option;
    pattern: string
  }
  include Jsonable.B with type t := t

end

module DocumentSelector : sig 
  type t = DocumentFilter.t array
  include Jsonable.B with type t := t

end

module WorkDoneProgressOptions : sig
  type t = {
    workDoneProgress : bool option
  }
  include Jsonable.B with type t := t

end

module TextDocumentRegistrationOptions : sig
  type document_selector = DocumentSelector of DocumentSelector.t | Null
  type t = {
    documentSelector : document_selector
  }
  include Jsonable.B with type t := t

end

module DefinitionRegistrationOptions : sig
  type t
  include Jsonable.B with type t := t

end

module TextDocumentIdentifier : sig 
  type t = { uri : DocumentUri.t }
  include Jsonable.B with type t := t

end

module Position : sig
  type t = {
    line : int;
    character : int
  }
  include Jsonable.B with type t := t

end

module TextDocumentPositionParams : sig
  type t = {
    textDocument : TextDocumentIdentifier.t;
    position : Position.t
  }
  include Jsonable.B with type t := t

end

module PartialResultParams : sig
  type t = {partialResultToken : ProgressToken.t option}
  include Jsonable.B with type t := t

end

module DefinitionParams : sig
  type t = {
    partialResultToken : ProgressToken.t option;
    textDocument : TextDocumentIdentifier.t;
    position : Position.t;
    work_done_token : ProgressToken.t option
  }
  include Jsonable.B with type t := t

end

module Range : sig 
  type t = {
    start : Position.t;
    end_ : Position.t
  }
  include Jsonable.B with type t := t

end

module Location : sig 
  type t = {
    uri : DocumentUri.t;
    range : Range.t
  }
  include Jsonable.B with type t := t

end
 
module DiagnosticSeverity : sig 
  type t = Error | Warning | Information | Hint
  include Jsonable.B with type t := t
end

module CodeDescription : sig 
  type t = {
    href : URI.t
  }
  include Jsonable.B with type t := t
end

module DiagnosticTag : sig
  type t = Unnecessary | Deprecated
  include Jsonable.B with type t := t
end

module DiagnosticRelatedInformation : sig
  type t = {
    location : Location.t;
    message : string
  }
  include Jsonable.B with type t := t
end

module Diagnostic : sig
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
  include Jsonable.B with type t := t
end 