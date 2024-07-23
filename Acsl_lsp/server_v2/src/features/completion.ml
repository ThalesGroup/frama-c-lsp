(* let acsl_keywords = 
  [
    ("admits", Types.CompletionItemKind.Folder);
    ("assert", Types.CompletionItemKind.Folder);
    ("assigns", Types.CompletionItemKind.Folder);
    ("assumes", Types.CompletionItemKind.Folder);
    ("allocates", Types.CompletionItemKind.Folder);
    ("axiom", Types.CompletionItemKind.Function);
    ("axiomatic", Types.CompletionItemKind.Function);
    ("behavior", Types.CompletionItemKind.Function);
    ("behaviors", Types.CompletionItemKind.Function);
    ("breaks", Types.CompletionItemKind.Function);
    ("case", Types.CompletionItemKind.Function);
    ("char", Types.CompletionItemKind.Function);
    ("checks", Types.CompletionItemKind.Function);
    ("complete", Types.CompletionItemKind.Function);
    ("continues", Types.CompletionItemKind.Function);
    ("decreases", Types.CompletionItemKind.Function);
    ("disjoint", Types.CompletionItemKind.Function);
    ("double", Types.CompletionItemKind.Function);
    ("else", Types.CompletionItemKind.Function);
    ("ensures", Types.CompletionItemKind.Function);
    ("enum", Types.CompletionItemKind.Function);
    ("exits", Types.CompletionItemKind.Function);
    ("float", Types.CompletionItemKind.Function);
    ("for", Types.CompletionItemKind.Function);
    ("frees", Types.CompletionItemKind.Function);
    ("if", Types.CompletionItemKind.Function);
    ("inductive", Types.CompletionItemKind.Function);
    ("int", Types.CompletionItemKind.Function);
    ("integer", Types.CompletionItemKind.Function);
    ("invariant", Types.CompletionItemKind.Function);
    ("global", Types.CompletionItemKind.Function);
    ("label", Types.CompletionItemKind.Function);
    ("lemma", Types.CompletionItemKind.Function);
    ("logic", Types.CompletionItemKind.Function);
    ("long", Types.CompletionItemKind.Function);
    ("loop", Types.CompletionItemKind.Function);
    ("pragma", Types.CompletionItemKind.Function);
    ("predicate", Types.CompletionItemKind.Function);
    ("reads", Types.CompletionItemKind.Function);
    ("real", Types.CompletionItemKind.Function);
    ("requires", Types.CompletionItemKind.Function);
    ("returns", Types.CompletionItemKind.Function);
    ("short", Types.CompletionItemKind.Function);
    ("signed", Types.CompletionItemKind.Function);
    ("sizeof", Types.CompletionItemKind.Function);
    ("slice", Types.CompletionItemKind.Function);
    ("impact", Types.CompletionItemKind.Function);
    ("struct", Types.CompletionItemKind.Function);
    ("terminates", Types.CompletionItemKind.Function);
    ("type", Types.CompletionItemKind.Function);
    ("union", Types.CompletionItemKind.Function);
    ("unsigned", Types.CompletionItemKind.Function);
    ("variant", Types.CompletionItemKind.Function);
    ("void", Types.CompletionItemKind.Function);
  ]

(* returns the label, the kind and details of the completion item *)
  let find_completions (filename : string) (line : int) (ch : int) : (string * Types.CompletionItemKind.t) list =
    ignore filename;
    ignore line;
    ignore ch;
    (List.nth acsl_keywords 0)::(List.nth acsl_keywords 1)::(List.nth acsl_keywords 2)::(List.nth acsl_keywords 3)::[]
    (* acsl_keywords *)

let json_of_completions (completions : (string * Types.CompletionItemKind.t) list) : Json.t list = 
  List.map (fun (label, kind) ->
    Types.CompletionItem.json_of_t (Types.CompletionItem.create ~label:label ~kind:kind ~insertText:label ())
  ) completions

let completion_items (req : Types.RequestMessage.t) : Json.json = 
  let params = match req.params with 
    | Some p -> Types.CompletionParams.t_of_json p
    | None -> Settings.Self.debug ~level:1 "No completion params \n%!"; assert false
  in
  let uri = params.textDocument.uri in 
  let file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
  (* let pos = Utils.position_t_to_filepath_position file params.position in *)
  let completions = find_completions file params.position.line params.position.character in
  Types.ResponseMessage.json_of_t 
  (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:
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
let is_in_acsl_block (filename : string) (line : int) : bool =
  let ic = open_in filename in
  let start_line = ref 0 in
  let end_line = ref 0 in
  let cnt = ref (-1) in
  let curr_line = ref "" in
  let acsl_block_started = ref false in
  let res = ref false in
  Settings.Self.debug ~level:1 "Requested line: %d\n%!" line;

  try 
    while true do
      cnt := !cnt + 1;
      curr_line := Stdlib.input_line ic;
      match Utils.contains !curr_line ~suffix:"/*@" with
      | true ->  
          Settings.Self.debug ~level:1 "Found beginning of acsl block: %d\n%!" !cnt;
          start_line := !cnt;
          acsl_block_started := true;
      | false -> 
        (match Utils.contains !curr_line ~suffix:"*/" with 
        | true ->
            Settings.Self.debug ~level:1 "Found end of acsl block: %d\n%!" !cnt;
            end_line := !cnt;
            acsl_block_started := false; 
            res := !res || (line >= !start_line && line <= !end_line) ; (* todo : how to interrupt the loop when found *)
            (match !res with 
            | true -> Settings.Self.debug ~level:1 "Is in acsl block\n%!";
            | false -> Settings.Self.debug ~level:1 "Is not in acsl block\n%!";)
        | false -> ();)
        done;
        Stdlib.close_in ic;
        !res
    with _ -> 
      Settings.Self.debug ~level:1 "END OF FILE REACHED FOR ACSL BLOCK CHECK\n%!";
      Stdlib.close_in ic;
      !res

 *)
