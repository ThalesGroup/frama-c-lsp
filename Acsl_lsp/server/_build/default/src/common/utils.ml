let pos_is_within_range pos (pos1, pos2 : (Filepath.position * Filepath.position)) = 
  let curr_pos = pos.Filepath.pos_lnum + (pos.Filepath.pos_cnum - pos.Filepath.pos_bol) in 
  let min = pos1.Filepath.pos_lnum + (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol) in 
  let max = pos2.Filepath.pos_lnum + (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol) in 
  curr_pos >= min && curr_pos <= max

(* Extract the JSON part only from the request (removes the header part etc.) *)
let extract_json_from_request request =
  let start_index = String.index request '{' in
  let end_index = String.rindex request '}' in
  String.sub request start_index (end_index - start_index + 1)

let get = function Some v -> v | None -> invalid_arg "option is None";

open File

(* Initialize the file representation *)
let initialize_file (filename : string) : unit =
  let filepath = Filepath.Normalized.of_string filename in
  (* Now you have a normalized file path *)
  let file = from_filename filepath in
  init_from_c_files [file]

let get_ast_from_file file_content () =
  (* Initialize the file representation *)
  initialize_file file_content;
  (* Obtain the AST *)
  Ast.get ()