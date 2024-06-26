let max_int = (-1) lsr 1 (* stdlib function *)
let config_id = 123456789
let file_str f = 
  let dir_list = [f] in 
  let dir_string = Filepath.Normalized.to_string_list dir_list in 
  List.nth dir_string 0

let get_lsp_range ((pos1, pos2) : Cil_types.location) : Types.Range.t =
  Types.Range.create
    (Types.Position.create pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol))
    (Types.Position.create pos2.pos_lnum (pos2.pos_cnum - pos2.pos_bol))

let remove_file_scheme uri =
  let regex = Str.regexp {|file://|} in
  Str.global_replace regex "" uri

let remove_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex "" str

let remove_newline str = 
  let regex = Str.regexp {|.*|} in 
  ignore (Str.search_forward regex str 0);
  Str.matched_string str

let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

(* Converts all filenames into t type *)
let get_t_from_filename filename_list =
  let t_list = List.map (fun filename -> File.from_filename (Datatype.Filepath.of_string filename)) filename_list in
  t_list

let position_t_to_filepath_position (uri : Types.DocumentUri.t) (pos : Types.Position.t) : Filepath.position =
  let pos_path = Filepath.Normalized.of_string uri in
  let pos_lnum = pos.line in
  let pos_bol = 0 in
  let pos_cnum = pos.character in
  { Filepath.pos_path; pos_lnum; pos_bol; pos_cnum }

let find_word str ch =
  if (String.equal str "") then "" else
  let r = Str.regexp {|\b[_A-Za-z0-9]+\b|} in
  try 
    ignore(Str.search_backward r str ch);
    Str.matched_string str
  with Not_found -> ""

let read_line_from_file filename line_number =
  let ic = open_in filename in
  let cnt = ref 0 in
  let line = ref "" in
  while (!cnt <= line_number) do
    try 
      (*Printf.printf "curr_line_numb %d, given line_num %d \n%!"!cnt line_number;*)
      line := Stdlib.input_line ic;
      cnt := !cnt + 1;
    with _ -> Stdlib.close_in ic; (* close the file in case input_line fails *)
  done;
  Stdlib.close_in ic;
  (*Printf.printf "line = %s\n%!" !line;*)
  !line

(* Function to retrieve function call at given line and character index *)
let retrieve_symbol line_number character_index file_name =
  find_word (read_line_from_file file_name line_number) character_index 

let send_request client_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send client_sock response_bytes 0 (Bytes.length response_bytes) [] in
  ignore sent

let send_error_request err sock = 
  send_request sock (Json.save_string (Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Types.Int 465841231564) ~error:(Types.ResponseError.create ~code:(-32803) ~message:err ()) ());)) (* todo : give proper id *)