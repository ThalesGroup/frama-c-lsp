
(** 
    Description : 
    Module for Go To Declaration feature of the Language Server Protocol.

    Version : 1.0
    - Finds C function, variable, struct, union and enum declarations
    - If several declarations exist for the same func/var, it will go to the first include file listed at the beginning
*)

let glob_visitor symbol declarations = object 
  inherit Visitor.frama_c_inplace
    method !vglob_aux g =
      match g with 
      | GEnumTagDecl (ei,loc) -> 
        if (String.equal symbol ei.eorig_name) then
          declarations := loc :: !declarations;
        Cil.DoChildren
      
      | GCompTagDecl (ci,loc) -> 
        if (String.equal symbol ci.corig_name) then
          declarations := loc :: !declarations;
        Cil.DoChildren
      
      | GVarDecl (vi, loc) -> 
          (* if (String.equal symbol vi.vname) && (not vi.vformal) then *)
          (* if (not vi.vformal) 
            then Cil.DoChildren
          else *)
        if (String.equal symbol vi.vname) then 
        begin
          declarations := loc :: !declarations;
        end; 
        Cil.DoChildren
      
      | GFunDecl (_,vi,loc) -> 
        (* if (String.equal symbol vi.vname) && (not vi.vformal) then *)
        if (String.equal symbol vi.vname) then 
          begin
            declarations := loc :: !declarations;
          end;
        Cil.DoChildren

      (* | GVar (vi, _, loc) -> 
        if (String.equal symbol vi.vname) then
          begin
            declarations := loc :: !declarations;
          end;
        Cil.DoChildren
      
      | GFun (fd,loc) -> 
        if (String.equal symbol fd.svar.vname) then
          begin
            declarations := loc :: !declarations;
          end;
        Cil.DoChildren *)
    
      | _ -> Cil.DoChildren
  end


let vrbl_visitor symbol declarations = object 
  inherit Visitor.frama_c_inplace
  method! vlval (lh,_) = 
    match lh with 
    | Var v -> 
      (* Printf.printf "Lval : %s\n%!" (Pretty_utils.to_string Printer.pp_varinfo v); *)
      if (String.equal symbol v.vname) && (not v.vformal) then 
        declarations := v.vdecl :: !declarations;
      Cil.DoChildren;
    | Mem _ -> 
      (* Printf.printf "Exp : %s\n%!" (Pretty_utils.to_string Printer.pp_exp e); *)

      (* match e.enode with 
      | Const c -> 
          (match c with 
          | CStr s -> 
            if (String.equal symbol s) = true then 
              loca := Some e.eloc; Cil.DoChildren
          | _ -> Cil.DoChildren)
      | _ ->  *)
        Cil.DoChildren;
  end 

  

let decl_visitor symbol declarations = object 
  inherit Visitor.frama_c_inplace
  method! vvdec vi = 
    if (String.equal symbol vi.vname) then 
      declarations := vi.vdecl :: !declarations; Cil.DoChildren
  end

let iter_decl () = 
  Globals.Vars.iter_in_file_order (fun v _ -> 
      Printf.printf "Iter file order : %s\n%!" v.vname
    )   

let print_fxs file = 
  Printf.printf "Printing fxs ...\n%!";
  let fxs = Globals.FileIndex.get_functions ~declarations:true file in
  List.iter (fun (f : Cil_types.kernel_function) ->
    match f.fundec with 
    | Definition (fd, _) -> 
      Printf.printf "function definition name : %s\n%!" fd.svar.vname
    | Declaration (_,vi,_,_) -> 
      Printf.printf "function declaration name : %s\n%!" vi.vname
  ) fxs

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
  let declarations = ref [] in
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  

  Visitor.visitFramacFile (glob_visitor symbol declarations) (Ast.get ()); 
  Visitor.visitFramacFile (vrbl_visitor symbol declarations) (Ast.get ()); 

  !declarations

let retrieve_location2 (pos : Filepath.position) (cil_files : Cil_types.file list) =
  let declarations = ref [] in
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  
  let counter = ref 0 in
  List.iter (fun f ->
    counter := !counter + 1;
    Printf.printf "retrieve_location setting project : %s\n%!" ("file"^(Stdlib.string_of_int !counter));
    Project.set_current (Project.from_unique_name ("file"^(Stdlib.string_of_int !counter)));
    Visitor.visitFramacFile (glob_visitor symbol declarations) (f); 
    (* Printf.printf "Ast of %s : %s\n%!" ("file"^(Stdlib.string_of_int !counter)) (Pretty_utils.to_string Printer.pp_file f); *)

  ) cil_files;
  !declarations

let retrieve_location3 (pos : Filepath.position) new_globals =
  let declarations = ref [] in
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  

    List.iter(fun (v : Cil_types.varinfo) ->
      if (String.equal symbol v.vname) then 
        (
          Printf.printf "Symbol : %s, vname : %s\n%!" symbol v.vname;
          declarations := v.vdecl :: !declarations;
          match v.vdefined with 
          | true -> Printf.printf "New defined global : %s with location : %s\n%!" (Pretty_utils.to_string Printer.pp_varinfo v) (Pretty_utils.to_string Printer.pp_location v.vdecl)
          | false -> Printf.printf "New global : %s with location : %s\n%!" (Pretty_utils.to_string Printer.pp_varinfo v) (Pretty_utils.to_string Printer.pp_location v.vdecl)
        )
    ) !new_globals;

  !declarations

let create_lsp_locations (declarations) = 
  let res = List.map (fun (loc : Cil_types.location) ->
    (Lsp_types.Location.json_of_t
      (Lsp_types.Location.create 
        (Filepath.normalize (Filepath.Normalized.to_pretty_string (Stdlib.fst loc).pos_path))
        (Lsp_types.Range.create 
          (Lsp_types.Position.create ((Stdlib.fst loc).pos_lnum - 1) ((Stdlib.fst loc).pos_cnum - (Stdlib.fst loc).pos_bol))
          (Lsp_types.Position.create ((Stdlib.snd loc).pos_lnum - 1) ((Stdlib.snd loc).pos_cnum - (Stdlib.snd loc).pos_bol))
        )
      )
    )
  ) declarations in
  Json.of_list res
  

let find id declarationFile line ch sourceFiles rootPath : Json.json = 
  let pos = Utils.to_filepath_position declarationFile line ch in

  let new_globals = ref [] in 
  Cabs2cil.register_new_global_hook (fun vi _ -> 
    new_globals := vi :: !new_globals; 
    );

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
      (* let temp_files = (List.map (fun f -> File.from_filename (Filepath.Normalized.of_string f)) (get_workspace_files rootPath)) in
      let counter = ref 0 in
      let cil_files = List.map (fun f ->
        counter := !counter + 1;
        Printf.printf "find creating project : %s\n%!" ("file"^(Stdlib.string_of_int !counter));
        Project.set_current (Project.create ("file"^(Stdlib.string_of_int !counter)));
        Kernel.CppExtraArgs.set (extra_args);
        File.init_from_c_files [f];
        Ast.get ();
      ) temp_files in *)

  let locations = retrieve_location pos in

  if (List.length locations) = 0 then 
    Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:`Null ())
  else
    Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(create_lsp_locations locations) ())
  with exn -> Utils.make_error (Printexc.to_string (exn)) (id)

    