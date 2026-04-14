open Cil_types

let get_loc_info glob =
  let loc = Cil_datatype.Global.loc glob in
  let pos_start = fst loc in
  let line = pos_start.Filepath.pos_lnum in
  let file = Filepath.to_string pos_start.Filepath.pos_path in
  line, file

let ga_name = function
  | Dfun_or_pred (li, _) -> li.l_var_info.lv_name
  | Daxiomatic (s, _, _, _) -> s
  | Dtype (lti, _) -> lti.lt_name
  | Dlemma (s, _, _, _, _, _) -> s
  | Dinvariant (li, _) -> li.l_var_info.lv_name
  | Dtype_annot (li, _) -> li.l_var_info.lv_name
  | Dmodel_annot (mf, _) -> mf.mi_name
  | Dextended (e, _, _) -> "ACSL extension " ^ e.ext_name
  | _ -> ""

let compute_and_serialize id current_file_uri =
  let functions = ref ([] : Json.t list) in
  let globals = ref ([] : Json.t list) in
  let types = ref ([] : Json.t list) in
  let annotations = ref ([] : Json.t list) in
  
  let target_file_name = Filename.basename current_file_uri in

  let ast = Ast.get () in

  List.iter (fun glob ->
    let line, file = get_loc_info glob in
    let is_current_file = (Filename.basename file = target_file_name) in

    match glob with
    | GFun ({svar=vi; _}, _) | GFunDecl (_, vi, _) when is_current_file ->
        if not (List.exists (fun (item : Json.t) -> 
          match item with 
          | `Assoc l -> List.assoc "name" l = `String vi.vname
          | _ -> false) !functions) then
        functions := (`Assoc [
          ("name", `String vi.vname);
          ("type", `String "function");
          ("line", `Int line);
          ("file", `String file)
        ]) :: !functions

    | GVar (vi, _, _) | GVarDecl (vi, _) ->
        if is_current_file || not (Cil.global_is_in_libc glob) then
          globals := (`Assoc [
            ("name", `String vi.vname);
            ("type", `String "variable");
            ("line", `Int line);
            ("file", `String file)
          ]) :: !globals

    | GType (ti, _) ->
        types := (`Assoc [ ("name", `String ti.tname); ("type", `String "type"); ("line", `Int line) ; ("file", `String file)]) :: !types
    | GCompTag (ci, _) ->
        types := (`Assoc [ ("name", `String ci.cname); ("type", `String "type"); ("line", `Int line); ("file", `String file) ]) :: !types
    | GEnumTag (ei, _) ->
        types := (`Assoc [ ("name", `String ei.ename); ("type", `String "type"); ("line", `Int line); ("file", `String file) ]) :: !types

    | GAnnot (ga, _) when is_current_file ->
        let name = ga_name ga in
        if name <> "" then
          annotations := (`Assoc [
            ("name", `String name);
            ("type", `String "predicate");
            ("line", `Int line);
            ("file", `String file)
          ]) :: !annotations
    
    | _ -> ()
  ) ast.globals;

  let result_data : Json.t = `Assoc [
    ("functions", `List (List.rev !functions));
    ("globals", `List (List.rev !globals));
    ("types", `List (List.rev !types));
    ("annotations", `List (List.rev !annotations))
  ] in
  
  let lsp_message = Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_data () 
  in
  Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message)