open Cil_types

let contains_str str sub =
  let len_str = String.length str in
  let len_sub = String.length sub in
  let rec loop i =
    if i > len_str - len_sub then false
    else if String.sub str i len_sub = sub then true
    else loop (i + 1)
  in loop 0

let compute_and_serialize id _current_file_uri =
  let deps = Hashtbl.create 256 in
  
  let add_dep caller callee symbol =
    if callee <> "" && caller <> "" && symbol <> "" then
      let caller_base = Filename.basename caller in
      let callee_base = Filename.basename callee in
      (* On évite l'auto-dépendance et les fichiers systèmes/frama-c *)
      if caller_base <> callee_base && 
         not (contains_str callee "frama-c") && 
         not (contains_str callee "share") then
        begin
          let callee_map = 
            try Hashtbl.find deps caller_base 
            with Not_found -> 
              let m = Hashtbl.create 16 in 
              Hashtbl.add deps caller_base m; m 
          in
          let symbols = try Hashtbl.find callee_map callee_base with Not_found -> [] in
          if not (List.mem symbol symbols) then
            Hashtbl.replace callee_map callee_base (symbol :: symbols)
        end
  in

  let comp_files = Hashtbl.create 256 in
  let enum_files = Hashtbl.create 256 in
  let type_files = Hashtbl.create 256 in
  let logic_files = Hashtbl.create 256 in

  let ast = Ast.get () in

  List.iter (fun g ->
    let loc = Cil_datatype.Global.loc g in
    let file = Filepath.to_string (fst loc).Filepath.pos_path in
    match g with
    | GCompTag (ci, _) | GCompTagDecl (ci, _) -> Hashtbl.replace comp_files ci.ckey file
    | GEnumTag (ei, _) | GEnumTagDecl (ei, _) -> Hashtbl.replace enum_files ei.ename file
    | GType (ti, _) -> Hashtbl.replace type_files ti.tname file
    | GAnnot (ga, _) -> 
        (match ga with
         | Dfun_or_pred (li, _) -> Hashtbl.replace logic_files li.l_var_info.lv_name file
         | Dlemma (name, _, _, _, _, _) -> Hashtbl.replace logic_files name file
         | Dinvariant (li, _) -> Hashtbl.replace logic_files li.l_var_info.lv_name file
         | Dtype_annot (li, _) -> Hashtbl.replace logic_files li.l_var_info.lv_name file
         | _ -> ())
    | _ -> ()
  ) ast.globals;

  let visitor = object(_self)
    inherit Visitor.frama_c_inplace
    val mutable current_file = ""

    method! vglob_aux g =
      let loc = Cil_datatype.Global.loc g in
      current_file <- Filepath.to_string (fst loc).Filepath.pos_path;
      DoChildren

   method! vvrbl vi =
      let is_global = (vi.vstorage = Extern) || (vi.vstorage = Static && vi.vglob) in
      if is_global then begin
         let decl_file = Filepath.to_string (fst vi.vdecl).Filepath.pos_path in
         add_dep current_file decl_file vi.vname;
      end;
      DoChildren

    method! vtype typ =
      (match typ.tnode with
       | TComp ci -> (try add_dep current_file (Hashtbl.find comp_files ci.ckey) ci.cname with Not_found -> ())
       | TEnum ei -> (try add_dep current_file (Hashtbl.find enum_files ei.ename) ei.ename with Not_found -> ())
       | TNamed ti -> (try add_dep current_file (Hashtbl.find type_files ti.tname) ti.tname with Not_found -> ())
       | _ -> ());
      DoChildren

    method! vlogic_info_use li =
      let name = li.l_var_info.lv_name in
      (try add_dep current_file (Hashtbl.find logic_files name) name with Not_found -> ());
      DoChildren

    method! vlogic_var_use lv =
      (match lv.lv_origin with
       | Some vi -> 
           let decl_file = Filepath.to_string (fst vi.vdecl).Filepath.pos_path in
           add_dep current_file decl_file lv.lv_name
       | None -> ());
      DoChildren
  end in

  Visitor.visitFramacFileSameGlobals visitor ast;

  let json_deps = Hashtbl.fold (fun caller callee_map acc ->
    let callees_assoc = Hashtbl.fold (fun callee symbols acc_c ->
      (callee, `List (List.map (fun s -> `String s) symbols)) :: acc_c
    ) callee_map [] in
    (caller, `Assoc callees_assoc) :: acc
  ) deps [] in

  let result_data : Json.t = `Assoc json_deps in
  let lsp_message = Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_data () 
  in
  Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message)