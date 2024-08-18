
(** 
    Description : 
    Module for Go To Definition feature of the Language Server Protocol.

    Version : 1.0
    - Finds frama_c terms and predicates (valid_read_string, valid_string, minimum, maximum, ...)
    - Finds user defined terms and predicates
    - Finds C function, struct, union and enum definitions

*)

let glob_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
    method !vglob_aux g =
      match g with 
      | GEnumTag (ei,loc) -> 
        if (String.equal symbol ei.eorig_name) then
          loca := Some loc;
        Cil.DoChildren
      | GCompTag (ci,loc) -> 
        if (String.equal symbol ci.corig_name) then
          loca := Some loc;
        Cil.DoChildren
      | GType (ti,loc) -> 
        if (String.equal symbol ti.torig_name) then
          loca := Some loc;
        Cil.DoChildren
      | GVar (vi, _, loc) -> 
          (* Printf.printf "var : %s, symbol : %s \n%!" vi.vname symbol; *)
          if (String.equal symbol vi.vname) then
          begin
            loca := Some loc;
          end;
        Cil.DoChildren
      | GText _ -> 
        Cil.DoChildren
      | GFun (fd,loc) -> 
        if (String.equal symbol fd.svar.vname;) then
          begin
            (* Printf.printf "fun : %s\n%!" fd.svar.vname; *)
            loca := Some loc;
          end;
        Cil.DoChildren
      | GAsm (s,loc) -> 
        if (String.equal symbol s) then
          loca := Some loc;
        Cil.DoChildren
      | GPragma (_,_) -> 
        (* Printf.printf "Pragma : %s\n%!" (Pretty_utils.to_string Printer.pp_global g); *)
        Cil.DoChildren
      | GAnnot (ga, _) -> 
        (match ga with 
        | Dinvariant (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              (* Printf.printf "invariant : %s\n%!" li.l_var_info.lv_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren;
        | Dtype (lti, loc) -> 
          if (String.equal symbol lti.lt_name) then
            begin
              (* Printf.printf "logic type : %s\n%!" lti.lt_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dtype_annot (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              (* Printf.printf "type annot : %s\n%!" li.l_var_info.lv_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dfun_or_pred (li,loc) ->
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              (* Printf.printf "fun or pred : %s\n%!" li.l_var_info.lv_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dlemma (str,_,_,_,_,loc) ->
          (* Printf.printf "lemma : %s\n%!" str; *)
          if (String.equal symbol str) then
            begin
              loca := Some loc;
            end;
          Cil.DoChildren
        | _ -> ();
        Cil.DoChildren)
      | _ -> Cil.DoChildren
  end

let vrbl_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
  method! vvdec v = 
    if (String.equal symbol v.vname) = true then 
      begin
        loca := Some v.vdecl; Cil.DoChildren
      end
    else
    Cil.DoChildren;
  end 
 

let func_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
  method! vfunc v = 
    if (String.equal symbol v.svar.vname) = true then 
      begin
        loca := Some v.svar.vdecl; Cil.DoChildren
      end
    else
    Cil.DoChildren;
  end 

let print_attrs () = 
  Cil.iterGlobals (Ast.get ()) (fun glob -> 
    List.iter (fun (attr : Cil_types.attribute) -> 
      match attr with 
      | Attr (name, params) -> 
        Printf.printf "attribute : %s\n%!" name;
        List.iter (fun (param : Cil_types.attrparam) ->
          Printf.printf "Attr param : %s\n%!" (Pretty_utils.to_string Printer.pp_attrparam param)
        ) params;
      | _ -> ()
    ) (Cil.global_attributes glob)
  )


let get_workspace_files rootPath : string list = 
  if (String.equal rootPath "") then 
    (Settings.Self.debug ~level:0 "No source files and no root path provided.\n%!"; assert false)
  else
  let c_files = ref [] in
  let rec init_files_rec path = 
    let curr_files = ref [] in 
    (* read all files and folders of current directory *)
    let filenames = Array.to_list (Filepath.readdir (Filepath.Normalized.of_string path)) in
    (* make paths absolute *)
    curr_files := List.append !curr_files (List.map(fun x ->
      path^"/"^x
    ) filenames);
    (* remove non source files *)
    c_files := List.append !c_files (List.filter (fun x -> String.ends_with ~suffix:".c" x) (!curr_files));
    (* call the function recursively if folders were found in the current directory *)
    let folders = List.filter (fun x -> Sys.is_directory x) !curr_files in
    List.iter (fun folder ->
      init_files_rec (folder)
    ) folders;
  in
  init_files_rec rootPath;
  !c_files

let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 
  (* print_attrs (); *)
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  
  
  Visitor.visitFramacFile (glob_visitor loca symbol) (Ast.get ()); 
  
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 

let find id definitionFile line ch sourceFiles rootPath: Json.json =
  let pos = Utils.to_filepath_position definitionFile line ch in
  
  try 
    if not (String.equal sourceFiles "") then 
      (File.init_from_c_files 
      (List.map (fun x -> File.from_filename 
        (Filepath.Normalized.of_string x)) 
        (List.filter (fun x -> (String.ends_with ~suffix:".c" x) || (String.ends_with ~suffix:".h" x))
          (List.map (fun x -> (x)) 
            (String.split_on_char ' ' sourceFiles)
          )
        )
      );)
    else
      File.init_from_c_files (List.map (fun f -> File.from_filename (Filepath.Normalized.of_string f)) (get_workspace_files rootPath));

  (* Cil.iterGlobals (Ast.get ()) (fun g -> Printf.printf "Curr global : %s\n%!" (Pretty_utils.to_string Printer.pp_global g)); *)
  
  let (pos1, pos2) = retrieve_location pos in

  if pos1 = pos2 then 
    Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:`Null ())
  else
    Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:
      (Lsp_types.Location.json_of_t
        (Lsp_types.Location.create 
          (Filepath.normalize (Filepath.Normalized.to_pretty_string pos1.pos_path))
          (Lsp_types.Range.create (Lsp_types.Position.create (pos1.pos_lnum - 1) (pos1.pos_cnum - pos1.pos_bol))
            (Lsp_types.Position.create (pos2.pos_lnum - 1) (pos2.pos_cnum - pos2.pos_bol))
          )
        )
      )
      ()
    )
  with exn -> Utils.make_error (Printexc.to_string (exn)) (id)
