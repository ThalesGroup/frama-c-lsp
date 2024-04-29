
(* {1 document/definition} *)
module LocationLink = struct
  type t =
    { originSelectionRange : Json.t option [@default None]
    ; targetUri : Json.t
    ; targetRange : Json.t
    ; targetSelectionRange : Json.t
    }
end

(* {1 DocumentSymbols} *)
module DocumentSymbol = struct
  type t =
    { name : string
    ; detail : string option [@default None]
    ; kind : int
    ; tags : int list option [@default None]
    ; deprecated : bool option [@default None]
    ; range : Json.t
    ; selectionRange : Json.t
    ; children : t list option [@default None]
    }
end

(* Not used as of today, superseded by DocumentSymbol *)
module Location = struct
  type t =
    { uri : Json.t
    ; range : Json.t
    }
end

(* Not used as of today, superseded by DocumentSymbol *)
module SymInfo = struct
  type t =
    { name : string
    ; kind : int
    ; location : Location.t
    }
end

(* {1 Hover} *)

module HoverContents = struct
  type t =
    { kind : string
    ; value : string
    }
end

module HoverInfo = struct
  type t =
    { contents : HoverContents.t
    ; range : Json.t option [@default None]
    }
end

(* {1 Completion} *)

module LabelDetails = struct
  type t 
end

module TextEditReplace = struct
  type t =
    { insert : Json.t
    ; replace : Json.t
    ; newText : string
    }
end

module CompletionData = struct
  type t =
    { label : string
    ; insertText : string option [@default None]
    ; labelDetails : LabelDetails.t option [@default None]
    ; textEdit : TextEditReplace.t option [@default None]
    ; commitCharacters : string list option [@default None]
    }
end

(* Code Lenses *)
module Command = struct
  type t =
    { title : string
    ; command : string
    }
end

module CodeLens = struct
  type t =
    { range : Json.t
    ; command : Command.t option [@default None]
    }
end

(* SelectionRange *)
module SelectionRange = struct
  type t =
    { range : Json.t
    ; parent : t option [@default None]
    }
end

module ProgressToken : sig
  type t =
    | String of string
    | Int of int
end = struct
  type t =
    | String of string
    | Int of int
end

(* Pull Diagnostics *)
module DocumentDiagnosticParams = struct
  type t =
    { textDocument : string
    ; indentifier : string option [@default None]
    ; previousResultId : string option [@default None]
    ; workDoneToken : ProgressToken.t option [@default None]
    ; partialResultToken : ProgressToken.t option [@default None]
    }
end

module FullDocumentDiagnosticReport = struct
  type t =
    { kind : string
    ; resultId : string option [@default None]
    ; items : Json.t list
    }
end

module UnchangedDocumentDiagnosticReport = struct
  type t =
    { kind : string
    ; resultId : string option [@default None]
    }
end

module DocumentDiagnosticReportPartialResult = struct
  type t =
    { relatedDocuments :
        (Json.t * FullDocumentDiagnosticReport.t) list
    }
end
