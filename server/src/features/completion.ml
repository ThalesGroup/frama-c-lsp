 (*************************************************************************
 *                                                                        *
 *  This file is part of Frama-C/LSP plugin.                              *
 *                                                                        *
 *  You can redistribute it and/or modify it under the terms of the GNU   *
 *  General Public License as published by the Free Software Foundation,  *
 *  version 3.                                                            *
 *                                                                        *
 *  It is distributed in the hope that it will be useful,                 *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *
 *  GNU General Public License for more details.                          *
 *                                                                        *
 *  See the GNU General Public License version 3                          *
 *  for more details (enclosed in the file LICENSE.md                     *
 *                                                                        *
 *************************************************************************)

let acsl_keywords = 
  [
    ("admits", Lsp_types.CompletionItemKind.Folder);
    ("assert", Lsp_types.CompletionItemKind.Folder);
    ("assigns", Lsp_types.CompletionItemKind.Folder);
    ("assumes", Lsp_types.CompletionItemKind.Folder);
    ("allocates", Lsp_types.CompletionItemKind.Folder);
    ("axiom", Lsp_types.CompletionItemKind.Function);
    ("axiomatic", Lsp_types.CompletionItemKind.Function);
    ("behavior", Lsp_types.CompletionItemKind.Function);
    ("behaviors", Lsp_types.CompletionItemKind.Function);
    ("breaks", Lsp_types.CompletionItemKind.Function);
    ("case", Lsp_types.CompletionItemKind.Function);
    ("char", Lsp_types.CompletionItemKind.Function);
    ("checks", Lsp_types.CompletionItemKind.Function);
    ("complete", Lsp_types.CompletionItemKind.Function);
    ("continues", Lsp_types.CompletionItemKind.Function);
    ("decreases", Lsp_types.CompletionItemKind.Function);
    ("disjoint", Lsp_types.CompletionItemKind.Function);
    ("double", Lsp_types.CompletionItemKind.Function);
    ("else", Lsp_types.CompletionItemKind.Function);
    ("ensures", Lsp_types.CompletionItemKind.Function);
    ("enum", Lsp_types.CompletionItemKind.Function);
    ("exits", Lsp_types.CompletionItemKind.Function);
    ("float", Lsp_types.CompletionItemKind.Function);
    ("for", Lsp_types.CompletionItemKind.Function);
    ("frees", Lsp_types.CompletionItemKind.Function);
    ("if", Lsp_types.CompletionItemKind.Function);
    ("inductive", Lsp_types.CompletionItemKind.Function);
    ("int", Lsp_types.CompletionItemKind.Function);
    ("integer", Lsp_types.CompletionItemKind.Function);
    ("invariant", Lsp_types.CompletionItemKind.Function);
    ("global", Lsp_types.CompletionItemKind.Function);
    ("ghost", Lsp_types.CompletionItemKind.Function);
    ("label", Lsp_types.CompletionItemKind.Function);
    ("lemma", Lsp_types.CompletionItemKind.Function);
    ("logic", Lsp_types.CompletionItemKind.Function);
    ("long", Lsp_types.CompletionItemKind.Function);
    ("loop", Lsp_types.CompletionItemKind.Function);
    ("pragma", Lsp_types.CompletionItemKind.Function);
    ("predicate", Lsp_types.CompletionItemKind.Function);
    ("reads", Lsp_types.CompletionItemKind.Function);
    ("real", Lsp_types.CompletionItemKind.Function);
    ("requires", Lsp_types.CompletionItemKind.Function);
    ("returns", Lsp_types.CompletionItemKind.Function);
    ("short", Lsp_types.CompletionItemKind.Function);
    ("signed", Lsp_types.CompletionItemKind.Function);
    ("sizeof", Lsp_types.CompletionItemKind.Function);
    ("slice", Lsp_types.CompletionItemKind.Function);
    ("impact", Lsp_types.CompletionItemKind.Function);
    ("struct", Lsp_types.CompletionItemKind.Function);
    ("terminates", Lsp_types.CompletionItemKind.Function);
    ("type", Lsp_types.CompletionItemKind.Function);
    ("union", Lsp_types.CompletionItemKind.Function);
    ("unsigned", Lsp_types.CompletionItemKind.Function);
    ("variant", Lsp_types.CompletionItemKind.Function);
    ("void", Lsp_types.CompletionItemKind.Function);
  ]

(* returns the label, the kind and details of the completion item *)
  let find_completions (filename : string) (line : int) (ch : int) : (string * Lsp_types.CompletionItemKind.t) list =
    ignore filename;
    ignore line;
    ignore ch;
    (List.nth acsl_keywords 0)::(List.nth acsl_keywords 1)::(List.nth acsl_keywords 2)::(List.nth acsl_keywords 3)::[]
    (* acsl_keywords *)

let json_of_completions (completions : (string * Lsp_types.CompletionItemKind.t) list) : Json.t list = 
  List.map (fun (label, kind) ->
    Lsp_types.CompletionItem.json_of_t (Lsp_types.CompletionItem.create 
    ~label:label 
    ~kind:kind 
    ~insertText:label 
    ())
  ) completions

  (* todo : for the moment, this feature sends a dummy completion item list with four items in it. *)
  let completion_items id file line ch : Json.json = 

  (* let pos = Utils.position_t_to_filepath_position file params.position in *)
  let completions = find_completions file line ch in
  Lsp_types.ResponseMessage.json_of_t 
  (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:
    (Json.of_list (json_of_completions completions))
    ()
  )
