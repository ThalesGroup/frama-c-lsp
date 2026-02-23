open Cil_types

let property_to_string p =
  Format.asprintf "%a" Property.pretty p

let extract_acsl_spec kf =
  let spec = Annotations.funspec kf in
  let props = Property.ip_of_spec kf Kglobal ~active:[] spec in
  let requires = ref [] in
  let ensures = ref [] in
  List.iter (fun p ->
    match p with
    | Property.IPPredicate { ip_kind = Property.PKRequires _; _ } ->
        requires := (property_to_string p) :: !requires
    | Property.IPPredicate { ip_kind = Property.PKEnsures _; _ } ->
        ensures := (property_to_string p) :: !ensures
    | _ -> ()
  ) props;
  (!requires, !ensures)

let compute_and_serialize id _file =
  let functions = ref [] in
  let globals = ref [] in
  let types = ref [] in
  let predicates = ref [] in

  Globals.Functions.iter (fun kf ->
    if Kernel_function.is_definition kf then
      let (req, ens) = extract_acsl_spec kf in
      let func_json = `Assoc [
        ("name", `String (Kernel_function.get_name kf));
        ("type", `String "function");
        ("children", `List (
          (List.map (fun s -> `Assoc [("name", `String s); ("type", `String "requires")]) req) @
          (List.map (fun s -> `Assoc [("name", `String s); ("type", `String "ensures")]) ens)
        ))
      ] in
      functions := func_json :: !functions
  );

  Globals.Vars.iter (fun vi _init ->
    if not (Ast_types.is_fun vi.vtype) then
      globals := `Assoc [
        ("name", `String vi.vname); 
        ("type", `String "variable")
      ] :: !globals
  );

  let result_data = `Assoc [
    ("functions", `List !functions);
    ("globals", `List !globals);
    ("types", `List !types);
    ("predicates", `List !predicates)
  ] in

  let lsp_message = Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0" 
    ~id:(Lsp_types.Int id) 
    ~result:result_data 
    () 
  in

  let json_message = Lsp_types.ResponseMessage.json_of_t lsp_message in
  
  Json.save_string json_message