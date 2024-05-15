open Types
open Cil_types 

let get = function Some v -> v | None -> invalid_arg "option is None"

let read_line_at_number (filename : string) (line_number : int) : string option =
  let file = open_in filename in
  let rec read_lines_helper (line_num : int) : string option =
    if line_num <= 0 then (
      close_in file;
      None
    ) else (
      try
        let line = input_line file in
        if line_num = 1 then (
          close_in file;
          Some line
        ) else (
          read_lines_helper (line_num - 1)
        )
      with
      | End_of_file ->
          close_in file;
          None
    )
  in
  read_lines_helper line_number

let print_predicates (ast : Cil_types.file) = 
  List.iter (fun glob ->
    match glob with 
    | GFun (fd, _) -> 
      List.iter (fun stmt ->
        match stmt.skind with 
        | Instr (instr) -> 
          (match instr with 
          | Code_annot (ca, _) -> 
            (match ca.annot_content with 
            | AAssert (_, tp) -> 
              List.iter (fun pred ->
                Printf.printf "pred : %s\n%!" pred
              ) tp.tp_statement.pred_name;
            | AInvariant (_, _, tp) -> 
              List.iter (fun pred ->
                Printf.printf "pred : %s\n%!" pred
              ) tp.tp_statement.pred_name;
            | _ -> Printf.printf ""
            )
          | _ -> Printf.printf "")
        | _ -> Printf.printf ""
      ) fd.sallstmts;
      | _ -> Printf.printf ""
  ) ast.globals
  

let is_same_uri (uri1 : string) (uri2 : Filepath.position) =
  (*Printf.printf "pos 1 = %s\n%!" uri1;
  Printf.printf "pos 2 = %s\n%!" (Filepath.Normalized.to_pretty_string uri2.Filepath.pos_path); *)
  uri1 = Filepath.Normalized.to_pretty_string uri2.Filepath.pos_path

let pos_is_within_range pos (pos1, pos2 : (Filepath.position * Filepath.position)) = 
  (*Printf.printf "expr name %s\n%!" (get_expr_name_from_pos (Filepath.Normalized.to_pretty_string pos.Filepath.pos_path) 138 60);
  Printf.printf "curr_pos uri: %s\n%!" (Filepath.Normalized.to_pretty_string pos.Filepath.pos_path);
  Printf.printf "curr_pos line : %d\n%!" pos.Filepath.pos_lnum;
  Printf.printf "curr_pos char : %d\n\n%!" (pos.Filepath.pos_cnum - pos.Filepath.pos_bol);

  Printf.printf "min_pos uri: %s\n%!" (Filepath.Normalized.to_pretty_string pos1.Filepath.pos_path);
  Printf.printf "min_pos line : %d\n%!" pos1.Filepath.pos_lnum;
  Printf.printf "min_pos char : %d\n\n%!" (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol);

  Printf.printf "max_pos uri: %s\n%!" (Filepath.Normalized.to_pretty_string pos2.Filepath.pos_path);
  Printf.printf "max_pos line : %d\n%!" pos2.Filepath.pos_lnum;
  Printf.printf "max_pos char : %d\n\n%!" (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol);*)

  let curr_pos = pos.Filepath.pos_lnum + (pos.Filepath.pos_cnum - pos.Filepath.pos_bol) in 
  let min = pos1.Filepath.pos_lnum + (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol) in 
  let max = pos2.Filepath.pos_lnum + (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol) in 
  curr_pos >= min && curr_pos <= max

(* Extract the JSON part only from the request (removes the header part etc.) *)
let extract_json_from_request request =
  let start_index = String.index request '{' in
  let end_index = String.rindex request '}' in
  String.sub request start_index (end_index - start_index + 1)


(* Function to recursively get all files with specified extensions in a directory *)
let rec get_files_with_extensions dir extensions =
  let entries = Array.to_list (Sys.readdir dir) in
  let full_paths = List.map (Filename.concat dir) entries in
  let is_regular_file path =
    try
      let stats = Unix.stat path in
      stats.Unix.st_kind = Unix.S_REG
    with Unix.Unix_error _ -> false
  in
  let files, subdirs =
    List.partition (fun path -> is_regular_file path && List.exists (Filename.check_suffix path) extensions) full_paths
  in
  let subdir_files =
    List.map (fun subdir -> get_files_with_extensions subdir extensions) (List.filter (fun path -> try Unix.(stat path).Unix.st_kind = Unix.S_DIR with Unix.Unix_error _ -> false) subdirs)
  in
  List.concat (files :: subdir_files)

(* Returns all .c and .h files located in a folder *)
let get_all_source_files dir =
  get_files_with_extensions dir [".c"; ".h"]

(* Converts all filenames into t type *)
let get_t_from_filename filename_list =
  let open Datatype.Filepath in
  let t_list = List.map (fun filename -> File.from_filename (of_string filename)) filename_list in
  t_list

(* Example usage
let () =
  let root_folder = "../server" in
  let extensions = [".c"; ".h"] in
  let source_files = get_files_with_extensions root_folder extensions in
  List.iter print_endline source_files
 *)

open File

let initialize_file (filename : string) : unit =
  ignore filename;
  (*let filepath = Filepath.Normalized.of_string filename in
  let file = from_filename filepath in*)
  let files = get_t_from_filename (get_all_source_files ".") in (* TODO : dir might not always be "." avoid hard coding *)
  Printf.printf "List length = %d\n%!" (List.length files);
  List.iter (fun file ->
    Printf.printf "file : %s\n" (File.get_name file);
  ) files;
  init_from_c_files files

let get_ast_from_file filename () =
  initialize_file filename;
  Ast.get ()  

let position_t_to_filepath_position (uri : DocumentUri.t) (pos : Position.t) : Filepath.position =
  let pos_path = Filepath.Normalized.of_string uri in
  let pos_lnum = pos.line in
  let pos_bol = 0 in
  let pos_cnum = pos.character (* default value *) in
  { Filepath.pos_path; pos_lnum; pos_bol; pos_cnum }

(* TODO : Returns the function's name (Some string) if the given position is 
  located between the beginning and the end of the predicate or logic expression's 
  name (between a space and and an opening parenthesis).
  Else returns None
*)

(* 
open Cil_types

Function to extract function calls from ACSL assigns annotations *)