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
  

let process_annotation g = 
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
  Printf.printf "FX NAME: %s\n%!" fun_name;
  let retrieved_fx = retrieve_function_call pos.pos_lnum (pos.pos_cnum - pos.pos_bol) 
  (Filepath.Normalized.to_pretty_string pos.pos_path) in
  String.compare (retrieved_fx) fun_name

(* todo : change the name of retrieve_acsl_annotations function *)
let retrieve_acsl_annotations (pos : Filepath.position) =
  let framac_share = "/home/user/.opam/4.13.1_fc28/share/frama-c/share" in (* todo : find user's frama-c share path : ?register option in plugin and launch with $(frama-c -print-share-path)*)
  Kernel.Share.set (Filepath.Normalized.of_string framac_share);
  let share = Kernel.Share.get () in
  Filepath.add_symbolic_dir framac_share share;
  Printf.printf "Share path = %s\n%!" (file_str share);

  let loca = ref None in 
  
  let li_visitor = object 
    inherit Visitor.frama_c_inplace
    method !vlogic_info_use li = 
      match li.l_body with 
      | LBpred pred -> 
        (*Cil_printer.pp_location Format.std_formatter pred.pred_loc;
        Format.pp_print_flush Format.std_formatter ();
        Printf.printf "\n%!"; *)
        if (compare_retrieved_function_name pos li.l_var_info.lv_name) = 0 then
          begin
            loca := Some pred.pred_loc; DoChildren
          end
        else 
        DoChildren
      | _ -> ();
      DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals li_visitor (Ast.get ());
  
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) (* todo : should return a json error instead of result *)

  (*let glob_visitor = object 
    inherit Visitor.frama_c_inplace
      method !vglob_aux g =
        match g with 
        | GEnumTagDecl (ei,_) -> 
          Printf.printf "Enum Tag Decl : %s\n%!" ei.eorig_name;
          DoChildren
        | GEnumTag (ei,_) -> 
          Printf.printf "Enum Tag : %s\n%!" ei.eorig_name;
          DoChildren
        | GCompTagDecl (ci,_) -> 
          Printf.printf "Comp Tag Decl : %s\n%!" ci.corig_name;
          DoChildren
        | GCompTag (ci,_) -> 
          Printf.printf "Comp Tag : %s\n%!" ci.corig_name;
          DoChildren
        | GType (ti,_) -> 
          Printf.printf "Type : %s\n%!" ti.torig_name;
          DoChildren
        | GVar (vi, _, _) -> 
          Printf.printf "Var name : %s\n%!" vi.vname;
          DoChildren
        | GVarDecl (vi, _) -> 
          Printf.printf "Var decl name : %s\n%!" vi.vname;
          DoChildren
        | GText s -> 
          Printf.printf "Text : %s\n%!" s;
          DoChildren
        | GFun (fd,_) -> 
          Printf.printf "Fun : %s\n%!" fd.svar.vname;
          DoChildren
        | GFunDecl (_,vi,_) -> 
          Printf.printf "Fun Decl. : %s\n%!" vi.vname;
          DoChildren
        | GAsm (s,_) -> 
          Printf.printf "Asm : %s\n%!" s;
          DoChildren
        | GPragma (_,loc) -> 
          Printf.printf "Pragma at : \n%!";
          Cil_printer.pp_location Format.std_formatter loc;
          Format.pp_print_flush Format.std_formatter () ;
          Printf.printf "\n%!";
          DoChildren
        | GAnnot (ga, loc) -> 
          Printf.printf "Annot : \n%!";
          process_annotation ga;
          Cil_printer.pp_location Format.std_formatter loc;
          Format.pp_print_flush Format.std_formatter () ;
          Printf.printf "\n%!";
          DoChildren
      end
    in
    let framac_share = "/home/user/.opam/4.13.1_fc28/share/frama-c/share" in
    Kernel.Share.set (Filepath.Normalized.of_string framac_share);
    let share = Kernel.Share.get () in
    Filepath.add_symbolic_dir framac_share share;
    Printf.printf "Share path = %s\n%!" (file_str share);
    Visitor.visitFramacFileSameGlobals glob_visitor (Ast.get ());*)

  (*let llabel_visitor = object 
    inherit Visitor.frama_c_inplace
      method !vlogic_label ll =
        match ll with 
        | FormalLabel x -> Printf.printf "Logic label : %s\n%!" x; DoChildren
        | _ -> DoChildren
      end
    in
  Visitor.visitFramacFileSameGlobals llabel_visitor (Ast.get ());*)

  (*let glob_visitor = object 
    inherit Visitor.frama_c_inplace
      method !vannotation g = 
        match g with 
        | Dfun_or_pred (li, (pos1, pos2)) -> 
            (* if the uri = global def uri and position is contained within definition range *)
            Printf.printf "Fun or pred = %s\n%!" li.l_var_info.lv_name;
            (*Printf.printf "expr_name = %s\n%!" expr_name;*)
            if (compare_retrieved_function_name pos li.l_var_info.lv_name) = 0
            then
              Printf.printf "comp result : %s, [%d:%d -> %d:%d] %s\n%!"
                li.l_var_info.lv_name
                pos1.Filepath.pos_lnum
                (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol)
                pos2.Filepath.pos_lnum
                (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol)
                (Filepath.Normalized.to_pretty_string pos1.Filepath.pos_path);
            DoChildren
        | Daxiomatic (str,_,_,_) -> 
          Printf.printf "Axiomatic : %s\n" str;
          DoChildren
        | Dlemma (str,_,_,_,_,_) -> 
          Printf.printf "Lemma : %s\n" str; 
          DoChildren
        | Dmodel_annot (mi,_) -> 
          Printf.printf "Model Annot : %s\n" mi.mi_name; 
          DoChildren
        | Dextended (acsl,_,_) -> 
          Printf.printf "ACSL Ext. : %s\n" acsl.ext_name; 
          DoChildren
        | Dinvariant (li,_) -> 
          Printf.printf "Invariant : %s\n" li.l_var_info.lv_name; 
          DoChildren
        | Dvolatile (_,_,_,_,_) -> 
          Printf.printf "Volatile :\n"; 
          DoChildren
        | Dtype_annot _-> 
          Printf.printf "Type Annot. :\n"; 
          DoChildren
        | _ -> 
          DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals glob_visitor (Ast.get ());*)

  (*let pred_visitor = object 
    inherit Visitor.frama_c_inplace
    method! vpredicate pred = 
        match pred.pred_content with 
        | Papp (li,_,_) -> 
          let loc = pred.pred_loc in
          (*Cil_printer.pp_predicate_node Format.std_formatter pred.pred_content;
          Format.pp_print_flush Format.std_formatter ();
          Printf.printf "\n%!";
          Cil_printer.pp_location Format.std_formatter loc;
          Format.pp_print_flush Format.std_formatter ();
          Printf.printf "\n%!";*)
          if (compare_retrieved_function_name pos li.l_var_info.lv_name) = 0 then 
            begin
              Printf.printf "\nPRED retrieved : %s, original : %s \n%!" 
              (get (retrieve_function_call pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (file_str pos.pos_path))) 
              li.l_var_info.lv_name; 
              Cil_printer.pp_location Format.std_formatter loc;
              Format.pp_print_flush Format.std_formatter ();
              let target_loc = Globals.Syntactic_search.find_in_scope li.l_var_info.lv_name Global in
              match target_loc with 
              | Some x -> Cil_printer.pp_location Format.std_formatter x.vdecl; DoChildren
              | None -> ();
              Format.pp_print_flush Format.std_formatter ();
              Printf.printf "\nEND OF TARGET LOC\n%!";
              DoChildren
            end
          
        else DoChildren
      (*| Pnot pr -> 
        Printf.printf "\n PRED NOT \n%!";
        Cil_printer.pp_location Format.std_formatter pr.pred_loc;
        Format.pp_print_flush Format.std_formatter ();
        DoChildren*)
      (*| Plet (li,pr) ->
        Printf.printf "\nPLET: %s\n%!" li.l_var_info.lv_name;
        Cil_printer.pp_location Format.std_formatter pr.pred_loc;
        Format.pp_print_flush Format.std_formatter ();
        DoChildren*)
      | _ -> DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals pred_visitor (Ast.get ());*)


  (*let term_visitor = object 
    inherit Visitor.frama_c_inplace
    method !vterm vt = 
      match vt.term_node with 
      | Tapp (li,_,_) -> 
        let loc = vt.term_loc in
        Printf.printf "\nTERM retrieved : %s, original : %s \n%!" (get (retrieve_function_call pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (file_str pos.pos_path))) li.l_var_info.lv_name; 
        Cil_printer.pp_location Format.std_formatter loc;
        Format.pp_print_flush Format.std_formatter ();
        if (compare_retrieved_function_name pos li.l_var_info.lv_name) = 0
        then 
          DoChildren
        else
          DoChildren
      | _ -> 
      DoChildren
    end 
  in 
  Visitor.visitFramacFile term_visitor (Ast.get ());*)
  (* Locate the function containing the given position *)
  (*let found = ref None in
  let annotations = ref [] in
  let do_annot (emitter : Emitter.t) (code_annot : code_annotation) : unit = 
    ignore emitter;
    annotations := !annotations@[code_annot];
  in
  let visitor = object
    inherit Visitor.frama_c_inplace
    method! vstmt_aux stmt =
      let (start,end_) = Cil_datatype.Stmt.loc stmt in
      if (pos_is_within_range pos (start, end_)) = true then
      Annotations.iter_code_annot do_annot stmt;
      DoChildren
  end
  in
  Visitor.visitFramacFileSameGlobals visitor (Ast.get ());*)
  (* Example usage
  let _ =
    match retrieve_function_call 134 44 "tests/math.h" with
    | Some function_call -> printf "Function call found: %s\n" function_call
    | None -> printf "Function call not found\n"  *)
  

  (*let instr_visitor = object 
    inherit Visitor.frama_c_inplace
    method !vinst vi = 
      match vi with 
      | Code_annot (_, loc) -> 
        Cil_printer.pp_location Format.std_formatter loc;
        Printf.printf "\n%!";
        DoChildren
      | _ -> 
      DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals instr_visitor (Ast.get ());*)


  (*let lv_decl_visitor = object 
    inherit Visitor.frama_c_inplace
    method !vlogic_var_decl lv = 
      Printf.printf "logic var decl = %s\n%!" lv.lv_name; DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals lv_decl_visitor (Ast.get ());*)

  (*let lval_visitor = object 
    inherit Visitor.frama_c_inplace
    method !vlval lval = 
      Cil_printer.pp_lval Format.std_formatter lval; Printf.printf "\n%!"; DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals lval_visitor (Ast.get ());*)

  (*let var_visitor = object inherit 
    Visitor.frama_c_inplace 
    method !vvrbl var = 
      match var with 
      | _ -> 
        (*let (start, end_) = var.vdecl in 
        if (pos_is_within_range pos (start, end_)) = true then *)
        if var.vdefined = true then
          Printf.printf "var name : %s\n%!" var.vname;
        DoChildren
  end
  in
  Visitor.visitFramacFileSameGlobals var_visitor (Ast.get ());*)



  (*match !found with
  | Some spec ->
    spec.spec_behavior
  | None -> failwith "No function found at the given position"*)

  

(*let get_logic_body_location_range (pos1 : Filepath.position) (pos2 : Filepath.position) : Location.t option =
  
  let range = Range.create (Position.create pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol)) (Position.create pos2.pos_lnum (pos2.pos_cnum - pos2.pos_bol)) () in
  Some (Location.create (Filepath.Normalized.to_pretty_string pos1.pos_path) range ())*)
  
(* TODO : Returns the extracted function's name (Some string) if the given position in a source file (file)
  (line, character) is located between the beginning (backslash) and the end (opening 
  parenthesis) of the predicate or logic expression's name. Else returns None.
  signature : get_logic_var_name : (file : string) -> (line : int) -> (character : int) -> (range : (Filepath.position * Filepath.position)) -> string option
*)
