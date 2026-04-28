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
  
  let add_dep caller callee =
    if callee <> "" && caller <> "" && caller <> callee &&
       not (contains_str callee "frama-c") && not (contains_str callee "share") then
      let current_deps = try Hashtbl.find deps caller with Not_found -> [] in
      if not (List.mem callee current_deps) then
        Hashtbl.replace deps caller (callee :: current_deps)
  in

  let comp_files = Hashtbl.create 256 in
  let enum_files = Hashtbl.create 256 in
  let type_files = Hashtbl.create 256 in
  let logic_files = Hashtbl.create 256 in (* NOUVEAU : Annuaire pour l'ACSL *)

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
      let decl_file = Filepath.to_string (fst vi.vdecl).Filepath.pos_path in
      add_dep current_file decl_file;
      DoChildren

    method! vtype typ =
      (match typ.tnode with
       | TComp ci -> (try add_dep current_file (Hashtbl.find comp_files ci.ckey) with Not_found -> ())
       | TEnum ei -> (try add_dep current_file (Hashtbl.find enum_files ei.ename) with Not_found -> ())
       | TNamed ti -> (try add_dep current_file (Hashtbl.find type_files ti.tname) with Not_found -> ())
       | _ -> ());
      DoChildren

    method! vlogic_info_use li =
      (* On interroge notre annuaire logique au lieu de chercher l_loc *)
      (try add_dep current_file (Hashtbl.find logic_files li.l_var_info.lv_name) with Not_found -> ());
      DoChildren

    (* 5. Intercepter l'utilisation des Variables Logiques (ACSL pointant vers du C) *)
    method! vlogic_var_use lv =
      (match lv.lv_origin with
       | Some vi -> 
           let decl_file = Filepath.to_string (fst vi.vdecl).Filepath.pos_path in
           add_dep current_file decl_file
       | None -> ());
      DoChildren
  end in

  Visitor.visitFramacFileSameGlobals visitor ast;

  let json_deps = Hashtbl.fold (fun caller callees acc ->
    let callees_json = `List (List.map (fun c -> `String (Filename.basename c)) callees) in
    (Filename.basename caller, callees_json) :: acc
  ) deps [] in

  let result_data : Json.t = `Assoc json_deps in
  
  let lsp_message = Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_data () 
  in
  Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message)