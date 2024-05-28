open Types
open Cil_types 

let get = Option.get 
let file_str = Filepath.Normalized.to_pretty_string

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

let remove_file_scheme uri =
  let prefix = "file://" in
  if String.length uri >= String.length prefix && String.sub uri 0 (String.length prefix) = prefix then
    String.sub uri (String.length prefix) (String.length uri - String.length prefix)
  else
    uri
  
let contains s1 s2 =
  let re = Str.regexp_string s2
  in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false

let is_same_line (pos : Filepath.position) (loc : location) = 
  let start_pos, end_pos = loc in
  pos.pos_lnum >= start_pos.pos_lnum && pos.pos_lnum <= end_pos.pos_lnum

let pos_is_within_range (pos : Filepath.position) (loc : location) : bool =
  let start_pos, end_pos = loc in
  let lines = is_same_line pos loc in 
  let pos_char = pos.pos_cnum - pos.pos_bol in 
  let start_char = start_pos.pos_cnum - start_pos.pos_bol in 
  let end_char = end_pos.pos_cnum - end_pos.pos_bol in 
  let chars = pos_char >= start_char && pos_char <= end_char in
  Printf.printf "p:%d, %d, s:%d, %d, e:%d, %d\n%!" pos.pos_lnum pos_char start_pos.pos_lnum start_char end_pos.pos_lnum end_char;
  lines && chars

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

open File

let initialize_file (filename : string) : unit =
  let files = get_t_from_filename (get_all_source_files filename) in (* TODO : dir might not always be "." *)
  Printf.printf "List length = %d\n%!" (List.length files);
  List.iter (fun file ->
    Printf.printf "file : %s\n" (File.get_name file);
  ) files;
  init_from_c_files files

let get_ast_from_file filename =
  initialize_file filename;
  Ast.get ()  

let position_t_to_filepath_position (uri : DocumentUri.t) (pos : Position.t) : Filepath.position =
  let pos_path = Filepath.Normalized.of_string uri in
  let pos_lnum = pos.line in
  let pos_bol = 0 in
  let pos_cnum = pos.character in
  { Filepath.pos_path; pos_lnum; pos_bol; pos_cnum }


let get_logic_vars_list (ast : Cil_types.file) : (logic_info * location) list =
  let res = ref [] in
  List.iter (fun (g : Cil_types.global) ->
      match g with
      | GAnnot (ga,_) ->
        (match ga with
         | Dfun_or_pred (li, loc) -> res := !res @ [(li, loc)]
         | _ -> res := !res)
      | _ -> res := !res
    ) ast.globals;
  !res

(* Function to find the substring starting with '\' or ' ' and ending with '(' or ' ' *)
let find_word str idx =
  let is_boundary_char c = c = '\\' || c = ' ' || c = '(' || c = ')' in
  let rec find_start i =
    if i < 0 || is_boundary_char (String.get str i) then i + 1
    else find_start (i - 1)
  in
  let rec find_end i =
    if i >= String.length str || is_boundary_char (String.get str i) then i
    else find_end (i + 1)
  in
  let start_idx = find_start idx in
  let end_idx = find_end idx in
  let res = String.sub str start_idx (end_idx - start_idx) in 
  res

let read_line_from_file filename line_number =
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
  read_lines ic 0

(* Function to retrieve function call at given line and character index *)
let retrieve_function_call line_number character_index file_name =
  match read_line_from_file file_name line_number with 
    | Some line -> find_word line character_index 
    | None -> failwith "Line not found" 
  

let get_annot_type g = 
  match g with 
  | Dfun_or_pred (li,_) -> 
    Printf.printf "_____Fun or pred at : %s\n%!" li.l_var_info.lv_name;
  | Daxiomatic (str,_,_,_) -> 
    Printf.printf "_____Axiomatic : %s\n%!" str;
  | Dlemma (str,_,_,tp,_,_) -> 
    Printf.printf "_____Lemma : %s\n%!" str; 
    match tp.tp_statement.pred_content with 
    | Papp (li,_,_) -> Printf.printf "__________pred : %s\n%!" li.l_var_info.lv_name;
    | _ -> (); ;
  | Dmodel_annot (mi,_) -> 
    Printf.printf "_____Model Annot : %s\n%!" mi.mi_name; 
  | Dextended (acsl,_,_) -> 
    Printf.printf "_____ACSL Ext. : %d\n%!" acsl.ext_id; 
  | Dinvariant (li,_) -> 
    Printf.printf "_____Invariant : %s\n%!" li.l_var_info.lv_name; 
  | Dvolatile (_,_,_,_,_) -> 
    Printf.printf "_____Volatile :\n%!"; 
  | Dtype_annot _-> 
    Printf.printf "_____Type Annot. :\n%!"; 
  | Dtype (lti,_) ->
    Printf.printf "_____Type . :%s\n%!" lti.lt_name

let compare_retrieved_function_name (pos : Filepath.position) fun_name = 
  let retrieved_fx = retrieve_function_call pos.pos_lnum (pos.pos_cnum - pos.pos_bol) 
  (Filepath.Normalized.to_pretty_string pos.pos_path) in
  String.compare (retrieved_fx) fun_name

