open Types
open Utils
open Printer_tag
  
(** 
    Description : 
    Module for Go To Definition feature of the Language Server Protocol.
    The main function here is find_def defined in Utils.

    Version : 1.0
    - Finds the definition of an ACSL logic function (called in the source code).
    - Only finds frama_c builtin(?) predicates (valid_read_string, valid_string, minimum, maximum, ...)

*)

let retrieve_location (pos : Filepath.position) =
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

  let glob_visitor = object 
    inherit Visitor.frama_c_inplace
      method !vglob_aux g =
        match g with 
        | GEnumTagDecl (ei,loc) -> 
          Printf.printf "Enum Tag Decl : %s\n%!" ei.eorig_name;
          if (compare_retrieved_function_name pos ei.eorig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GEnumTag (ei,loc) -> 
          Printf.printf "Enum Tag : %s\n%!" ei.eorig_name;
          if (compare_retrieved_function_name pos ei.eorig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GCompTagDecl (ci,loc) -> 
          Printf.printf "Comp Tag Decl : %s\n%!" ci.corig_name;
          if (compare_retrieved_function_name pos ci.corig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GCompTag (ci,loc) -> 
          Printf.printf "Comp Tag : %s\n%!" ci.corig_name;
          if (compare_retrieved_function_name pos ci.corig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GType (ti,loc) -> 
          Printf.printf "Type : %s\n%!" ti.torig_name;
          if (compare_retrieved_function_name pos ti.torig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GVar (vi, _, loc) -> 
          Printf.printf "Var name : %s\n%!" vi.vname;
          if (compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GVarDecl (vi, loc) -> 
          Printf.printf "Var decl name : %s\n%!" vi.vname;
          if (compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GText (s) -> 
          Printf.printf "Text : %s\n%!" s;
          DoChildren
        | GFun (fd,loc) -> 
          Printf.printf "Fun : %s\n%!" fd.svar.vname;
          if (compare_retrieved_function_name pos fd.svar.vname;) = 0 then
            loca := Some loc;
          DoChildren
        | GFunDecl (_,vi,loc) -> 
          Printf.printf "Fun Decl. : %s\n%!" vi.vname;
          if (compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GAsm (s,loc) -> 
          Printf.printf "Asm : %s\n%!" s;
          if (compare_retrieved_function_name pos s) = 0 then
            loca := Some loc;
          DoChildren
        | GPragma (_,_) -> 
          Printf.printf "Pragma at : \n%!";
          DoChildren
        | GAnnot (_, _) -> 
          Printf.printf "Annot : \n%!";
          DoChildren
      end
    in
    let framac_share = "/home/user/.opam/4.13.1_fc28/share/frama-c/share" in
    Kernel.Share.set (Filepath.Normalized.of_string framac_share);
    let share = Kernel.Share.get () in
    Filepath.add_symbolic_dir framac_share share;
    Printf.printf "Share path = %s\n%!" (file_str share);
    Visitor.visitFramacFileSameGlobals glob_visitor (Ast.get ());

  (*let llabel_visitor = object 
    inherit Visitor.frama_c_inplace
      method !vlogic_label ll =
        match ll with 
        | FormalLabel x -> Printf.printf "Logic label : %s\n%!" x; DoChildren
        | _ -> DoChildren
      end
    in
  Visitor.visitFramacFileSameGlobals llabel_visitor (Ast.get ());*)


  let pred_visitor = object 
    inherit Visitor.frama_c_inplace
    method! vpredicate pred = 
        match pred.pred_content with 
        | Papp (li,_,_) -> 
          if (compare_retrieved_function_name pos li.l_var_info.lv_name) = 0 then 
            begin
              let target_loc = Globals.Syntactic_search.find_in_scope li.l_var_info.lv_name Global in
              match target_loc with 
              | Some x -> Cil_printer.pp_location Format.std_formatter x.vdecl; DoChildren
              | None -> ();
              Format.pp_print_flush Format.std_formatter ();
              Printf.printf "\nEND OF TARGET LOC\n%!";
              DoChildren
            end
          
        else DoChildren
      | _ -> DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals pred_visitor (Ast.get ());
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 

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
  

let process_global (params : DefinitionParams.t) (g : Cil_types.global) =
  ignore g;
  let temp_uri = params.textDocument.uri in 
  let uri = remove_file_scheme temp_uri in
  let curr_pos = position_t_to_filepath_position uri params.position in
  let target_loc = Printer_tag.loc_to_localizable ?precise_col:(Some true) curr_pos in
  (*let vinfo =  Printer_tag.varinfo_of_localizable target_loc in *)
  match target_loc with 
  | Some loc -> Printf.printf "%s\n%!" (label loc)
  | None -> Printf.printf "No decl\n%!"

let find_def (file : Cil_types.file) (req : RequestMessage.t) : Json.json = 
    print_predicates file;
    Printf.printf "find_def called\n%!";
    let params = DefinitionParams.t_of_json (get req.params) in
    let uri = params.textDocument.uri in 
    let file = remove_file_scheme uri in
    let pos = position_t_to_filepath_position file params.position in
    let (pos1, pos2) = retrieve_location pos in 
    (* todo : is it necessary to display an error if a definition can't be found for the clicked position 
       a simple 'ctrl' shouldn't raise an error, error should be raised only if ...?
      ?*)
    if pos1 = pos2 then 
      ResponseMessage.json_of_t (ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~error:(ResponseError.create ~code:(-32803) ~message:"Definition not found." ()) ())
    else  
      ResponseMessage.json_of_t (ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:
        (Location.json_of_t
          (Location.create 
            (Filepath.Normalized.to_pretty_string pos1.pos_path)
            (Range.create (Position.create pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol))
              (Position.create pos2.pos_lnum (pos2.pos_cnum - pos2.pos_bol))
            )
          )
        )
        ()
      )
    

    
    
