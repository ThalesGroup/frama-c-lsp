open Cil_types

let property_to_string p = Format.asprintf "%a" Property.pretty p

let get_function_details id func_name =
  try
    let kf = Globals.Functions.find_by_name func_name in
    let spec = Annotations.funspec kf in
    let props = Property.ip_of_spec kf Kglobal ~active:[] spec in
    let children = List.filter_map (fun p ->
      let s = property_to_string p in
      let loc = Property.location p in
      let pos_start = fst loc in
      let line = pos_start.Filepath.pos_lnum in
      
      let file_path = (pos_start.Filepath.pos_path :> string) in
      
      match p with
      | Property.IPPredicate { ip_kind = Property.PKRequires _; _ } -> 
          Some (`Assoc [
            ("name", `String s); 
            ("type", `String "requires"); 
            ("line", `Int line);
            ("file", `String file_path) 
          ])
      | Property.IPPredicate { ip_kind = Property.PKEnsures _; _ } -> 
          Some (`Assoc [
            ("name", `String s); 
            ("type", `String "ensures"); 
            ("line", `Int line);
            ("file", `String file_path)
          ])
      | _ -> None
    ) props in
    let lsp_message = Lsp_types.ResponseMessage.create 
      ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(`List children) () 
    in
    Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message)
  with Not_found ->
    let lsp_message = Lsp_types.ResponseMessage.create 
      ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(`List []) () in
    Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message)


let is_function_global = function
  | GFun _ | GFunDecl _ -> true
  | _ -> false

let global_name = function
  | GFun ({svar = vi; _}, _) | GVar(vi, _, _) 
  | GFunDecl (_, vi, _) | GVarDecl(vi, _) -> vi.vname
  | _ -> ""

let ga_name = function
  | Dfun_or_pred (li, _) -> li.l_var_info.lv_name
  | Daxiomatic (s, _, _, _) -> s
  | Dtype (lti, _) -> lti.lt_name
  | Dlemma (s, _, _, _, _, _) -> s
  | Dinvariant (li, _) -> li.l_var_info.lv_name
  | Dtype_annot (li, _) -> li.l_var_info.lv_name
  | Dmodel_annot (mf, _) -> mf.mi_name
  | _ -> ""

let compute_and_serialize id _file =
  let functions = ref [] in
  let globals = ref [] in
  let types = ref [] in
  let annotations = ref [] in
  
  let ast = Ast.get () in

  List.iter (fun glob ->
    match glob with
    | GFun ({svar=vi; _}, _) | GFunDecl (_, vi, _) | GVarDecl (vi, _) when Ast_types.is_fun vi.vtype ->
        functions := (`Assoc [
          ("name", `String vi.vname);
          ("type", `String "function")
        ]) :: !functions

    | GVar (vi, _, _) | GVarDecl (vi, _) when not (Ast_types.is_fun vi.vtype) ->
        globals := (`Assoc [
          ("name", `String vi.vname);
          ("type", `String "variable")
        ]) :: !globals

    | GType (ti, _) ->
        types := (`Assoc [
          ("name", `String ti.tname);
          ("type", `String "type")
        ]) :: !types

    | GAnnot (ga, _) ->
        let name = ga_name ga in
        if name <> "" then
          annotations := (`Assoc [
            ("name", `String name);
            ("type", `String "predicate")
          ]) :: !annotations
    
    | _ -> ()
  ) ast.globals;

  let result_data = `Assoc [
    ("functions", `List !functions);
    ("globals", `List !globals);
    ("types", `List !types);
    ("annotations", `List !annotations)
  ] in
  
  let lsp_message = Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_data () 
  in
  Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message)