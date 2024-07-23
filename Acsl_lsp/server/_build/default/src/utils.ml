let max_int = (-1) lsr 1 (* stdlib function *)

let file_str f = 
  let dir_list = [f] in 
  let dir_string = Filepath.Normalized.to_string_list dir_list in 
  List.nth dir_string 0

let dummyLoc filename : Cil_types.location = 
  (
    {pos_path=(Filepath.Normalized.of_string filename); pos_lnum=0;  pos_bol=1; pos_cnum=1},
    {pos_path=(Filepath.Normalized.of_string filename); pos_lnum=0;  pos_bol=1; pos_cnum=1}
  )

let real_loc ((pos1 : Filepath.position) , (pos2 : Filepath.position)) : Cil_types.location = 
  (
    {pos_path=pos1.pos_path; pos_lnum=pos1.pos_lnum-1; pos_bol=pos1.pos_bol-1; pos_cnum=pos1.pos_cnum},
    {pos_path=pos2.pos_path; pos_lnum=pos2.pos_lnum-1; pos_bol=pos2.pos_bol-1; pos_cnum=pos2.pos_cnum}
  )

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
      line := Stdlib.input_line ic;
      cnt := !cnt + 1;
    with End_of_file -> Stdlib.close_in ic;
  done;
  Stdlib.close_in ic;
  !line

let contains s1 ~suffix:s2 =
  let re = Str.regexp_string s2 in
  try 
    ignore (Str.search_forward re s1 0); 
    true
  with Not_found -> false

(* Function to retrieve function call at given line and character index *)
let retrieve_symbol line_number character_index file_name =
  find_word (read_line_from_file file_name line_number) character_index 

let send_request client_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send client_sock response_bytes 0 (Bytes.length response_bytes) [] in
  ignore sent

let make_error err id = 
  Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Types.Int id) ~error:(Types.ResponseError.create ~code:(-32803) ~message:err ()) ())

let id_to_int id =
  match id with 
  | Types.Int i -> i 
  | Types.Str s -> Stdlib.int_of_string s 
  | Types.Null -> 0

let send_error_request id err sock = 
  send_request sock (Json.save_string (make_error err id)) (* todo : give proper id *)

let apply_escapes (s : string) : string = 
  let regex = Str.regexp "\\\n" in 
  let _ = Str.match_beginning () in
  Str.global_replace regex "\n" s

let get_whole_line (pos : Filepath.position) : Cil_types.location = 
  let file = file_str pos.pos_path in 
  let line = read_line_from_file file (pos.pos_lnum - 1) in
  (
    {pos_path=pos.pos_path; pos_lnum=pos.pos_lnum - 1; pos_bol=pos.pos_bol; pos_cnum=pos.pos_cnum},
    {pos_path=pos.pos_path; pos_lnum=pos.pos_lnum - 1; pos_bol=pos.pos_bol; pos_cnum=pos.pos_cnum + (String.length line)}
  )

let split_key_value ss = 
  List.map (fun s ->
    let list = String.split_on_char ':' s in 
    (List.nth list 0), (Some (List.nth list 1))
  ) ss

let get_warn_status_of_string s =
  match s with 
  | "inactive" -> Log.Winactive 
  | "feedback_once" -> Log.Wfeedback_once 
  | "feedback" -> Log.Wfeedback 
  | "once" -> Log.Wonce 
  | "active" -> Log.Wactive 
  | "error_once" -> Log.Werror_once 
  | "error" -> Log.Werror 
  | "abort" -> Log.Wabort 
  | _ -> Log.Wactive (* note : default behavior *)

