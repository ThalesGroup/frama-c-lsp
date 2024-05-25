type id_ = Int of int | Str of string | Null

module Message : sig
  type t = {jsonrpc : string}
  include Jsonable.B with type t := t

end

module RequestMessage : sig
  type t = {
    jsonrpc : string;
    id : id_;
    method_ : string;
    params : Json.t option (* todo : array or object *)
  }
  include Jsonable.B with type t := t
end

module ResponseError : sig
  type t = {
    code: int; (* todo : implement errorcodes *)
    message: string;
    data: Json.t option
  }
  include Jsonable.B with type t := t
  val create : code:int -> message:string -> ?data:Json.t -> unit -> t
end

module ResponseMessage : sig
  type t = {
    jsonrpc : string;
    id : id_;
    result : Json.t  option;
    error : ResponseError.t option
  }
  include Jsonable.B with type t := t
  val create : jsonrpc:string -> id:id_ -> ?result:Json.t -> ?error:ResponseError.t -> unit -> t
end

module NotificationMessage : sig
  type t = {
    jsonrpc : string;
    method_ : string;
    params : Json.t array option
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
  type t = string
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
    work_done_token : ProgressToken.t option; 
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
    initialization_options : Json.t option;
    capabilities : ClientCapabilities.t;
    trace : TraceValue.t option;
    workspace_folders : (WorkspaceFolder.t array) option;
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
  type t = {registrations : Registration.t array}
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

module UnregistrationParams : sig
  type t = {unregistrations : Unregistration.t array}
  include Jsonable.B with type t := t
end

module TextDocumentSyncKind : sig
  type t = None | Full | Incremental
  include Jsonable.B with type t := t
end

module TextDocumentSyncOptions : sig 
  type t = {
    openClose : bool option;
    change : TextDocumentSyncKind.t option
  }
  include Jsonable.B with type t := t
end

module TextDocumentItem : sig
  type t = {
    uri : DocumentUri.t;
    languageId : string;
    version : int;
    text : string;
  }
  include Jsonable.B with type t := t
end

module DidOpenTextDocumentParams : sig
  type t = {
    textDocument : TextDocumentItem.t
  }  
  include Jsonable.B with type t := t
end

module TextDocumentChangeRegistrationOptions : sig
  type t = {syncKind : TextDocumentSyncKind.t}
  include Jsonable.B with type t := t
end

module VersionedTextDocumentIdentifier : sig
  type t = {
    uri : DocumentUri.t;
    version : int
  }
  include Jsonable.B with type t := t
end

module Position : sig
  type t = {
    line : int;
    character : int
  }

  val create : int -> int -> t

  include Jsonable.B with type t := t

end

module Range : sig 
  type t = {
    start : Position.t;
    end_ : Position.t
  }

  val create : Position.t -> Position.t -> t
  include Jsonable.B with type t := t

end

module TextDocumentContentChangeEvent : sig
  type t =
  | RangeChange of { range: Range.t; rangeLength: int option; text: string }
  | FullTextChange of { text: string }
  include Jsonable.B with type t := t
end

module DidChangeTextDocumentParams : sig
  type t = {
    textDocument : VersionedTextDocumentIdentifier.t;
    contentChanges : TextDocumentContentChangeEvent.t array
  }
  include Jsonable.B with type t := t
end

module TextDocumentIdentifier : sig 
  type t = { uri : DocumentUri.t }
  include Jsonable.B with type t := t

end

module DidCloseTextDocumentParams : sig
  type t = {textDocument : TextDocumentIdentifier.t }
  include Jsonable.B with type t := t
end

module DefinitionOptions : sig
  type t = {workDoneProgress: bool option}
  include Jsonable.B with type t := t
end

module ServerCapabilities : sig
  type definition_provider = Bool of bool | DefinitionOptions of DefinitionOptions.t
  type t = {
    definitionProvider : definition_provider option
  }

  val create : ?definitionProvider: definition_provider -> unit -> t

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

  val create_serverInfo : name:string -> ?version:string -> unit -> server_info
  val create : ?capabilities:ServerCapabilities.t -> ?serverInfo:server_info -> unit -> t

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

module Location : sig 
  type t = {
    uri : DocumentUri.t;
    range : Range.t
  }

  val create : DocumentUri.t -> Range.t -> t

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

module Command : sig
  type t = {
    title : string;
    command : string;
    arguments : Json.t list option
  }
  include Jsonable.B with type t := t
end