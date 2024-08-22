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

let completion_items id file line ch : Json.json = 

  (* let pos = Utils.position_t_to_filepath_position file params.position in *)
  let completions = find_completions file line ch in
  Lsp_types.ResponseMessage.json_of_t 
  (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:
    (Json.of_list (json_of_completions completions))
    ()
  )










(* pseudo code :
  for line in file :
    if line contains "/*@":
      start_line = line
    if line contains "*/":
      end_line = line
    
    return pos.line >= start_line && pos.line <= end_line
*)
(* let is_in_acsl_block (filename : string) (line : int) : bool =
  let ic = open_in filename in
  let start_line = ref 0 in
  let end_line = ref 0 in
  let cnt = ref (-1) in
  let curr_line = ref "" in
  let acsl_block_started = ref false in
  let res = ref false in
  Settings.Self.debug ~level:4 "Requested line: %d\n%!" line;

  try 
    while true do
      cnt := !cnt + 1;
      curr_line := Stdlib.input_line ic;
      match Utils.contains !curr_line ~suffix:"/*@" with
      | true ->  
          Settings.Self.debug ~level:4 "Found beginning of acsl block: %d\n%!" !cnt;
          start_line := !cnt;
          acsl_block_started := true;
      | false -> 
        (match Utils.contains !curr_line ~suffix:"*/" with 
        | true ->
            Settings.Self.debug ~level:4 "Found end of acsl block: %d\n%!" !cnt;
            end_line := !cnt;
            acsl_block_started := false; 
            res := !res || (line >= !start_line && line <= !end_line) ; (* todo : how to interrupt the loop when found *)
            (match !res with 
            | true -> Settings.Self.debug ~level:4 "Is in acsl block\n%!";
            | false -> Settings.Self.debug ~level:4 "Is not in acsl block\n%!";)
        | false -> ();)
        done;
        Stdlib.close_in ic;
        !res
    with _ -> 
      Settings.Self.debug ~level:4 "END OF FILE REACHED FOR ACSL BLOCK CHECK\n%!";
      Stdlib.close_in ic;
      !res *)

 
