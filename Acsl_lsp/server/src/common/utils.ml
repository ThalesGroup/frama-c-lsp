(* utils module *)
let get = Option.get 
let file_str f = 
  let dir_list = [f] in 
  let dir_string = Filepath.Normalized.to_string_list dir_list in 
  List.nth dir_string 0

let remove_file_scheme uri =
  let prefix = "file://" in
  if String.length uri >= String.length prefix && String.sub uri 0 (String.length prefix) = prefix then
    String.sub uri (String.length prefix) (String.length uri - String.length prefix)
  else
    uri
  
(* Extract the JSON part only from the request (removes the header part etc.) *)
let extract_json_from_request request =
  let start_index = String.index request '{' in
  let end_index = String.rindex request '}' in
  String.sub request start_index (end_index - start_index + 1)


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
  (*Printf.printf "curr_line_numb %d, given line_num %d \n%!"!cnt line_number;*)
  line := Stdlib.input_line ic;
  cnt := !cnt + 1;
done;
Stdlib.close_in ic;
(*Printf.printf "line = %s\n%!" !line;*)
!line

(*let read_line_from_file filename line_number =
  let ic = open_in filename in
  let rec read_lines ic current_line =
    try
      let line = input_line ic in
      if current_line = line_number then
        Some line
      else
        read_lines ic (current_line + 1)
    with
    | End_of_file ->
        close_in ic;
        None
  in
  read_lines ic 0*)

(* Function to retrieve function call at given line and character index *)
let retrieve_function_call line_number character_index file_name =
  find_word (read_line_from_file file_name line_number) character_index 
  

let compare_retrieved_function_name (pos : Filepath.position) fun_name = 
  let retrieved_fx = retrieve_function_call pos.pos_lnum (pos.pos_cnum - pos.pos_bol) 
  (Filepath.Normalized.to_pretty_string pos.pos_path) in
  String.compare (retrieved_fx) fun_name

