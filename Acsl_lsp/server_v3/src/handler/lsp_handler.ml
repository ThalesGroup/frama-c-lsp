
let receivedShutdown = ref false
let rootPath = ref ""
let remove_file_scheme uri =
  let regex = Str.regexp {|file://|} in
  Str.global_replace regex "" uri

let remove_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex "" str

let remove_newline str = 
  let regex = Str.regexp {|.*|} in 
  ignore (Str.search_forward regex str 0);
  Str.matched_string str

let find_word str ch =
  if (String.equal str "") then "" else
  let r = Str.regexp {|\b[_A-Za-z0-9]+\b|} in
  try 
    ignore(Str.search_backward r str ch);
    Str.matched_string str
  with Not_found -> ""
  
let make_error err id = 
  Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~error:(Lsp_types.ResponseError.create ~code:(-32803) ~message:err ()) ())
    
let read_line_from_file filename line_number =
  let ic = open_in filename in
  let cnt = ref 0 in
  let line = ref "" in
  while (!cnt <= line_number) do
    try 
      line := Stdlib.input_line ic;
      cnt := !cnt + 1;
    with End_of_file -> Stdlib.close_in ic;
  done;
  Stdlib.close_in ic;
  !line

let retrieve_symbol line_number character_index file_name =
  find_word (read_line_from_file file_name line_number) character_index 

let id_to_int id =
  match id with 
  | Lsp_types.Int i -> i 
  | Lsp_types.Str s -> Stdlib.int_of_string s 
  | Lsp_types.Null -> 0

let id_to_str id =
  match id with 
  | Lsp_types.Int i -> Stdlib.string_of_int i
  | Lsp_types.Str s -> s 
  | Lsp_types.Null -> ""

let is_result json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "result") fields)
    | _ -> Printf.printf "no result\n%!"; false
  with
  | Json.Error _ -> false

let is_error json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "error") fields)
    | _ -> Printf.printf "no error\n%!"; false

  with
  | Json.Error _ -> false

let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      not (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Printf.printf "no notif\n%!"; false

  with
  | Json.Error _ -> false

let is_request json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Printf.printf "no request\n%!"; false

  with
  | Json.Error _ -> false
  
let cpp_extra_args () = 
  let includePaths = 
    List.map (fun x -> "-I"^(!rootPath^"/"^(Filename.basename x))) (!Configuration.global_params.includePaths)
  in
  let macros = List.map (fun x -> "-D"^x) (!Configuration.global_params.macros) in
  let res = " -cpp-extra-args=\""^(String.concat " " includePaths)^(String.concat " " macros)^"\"" in 
  res

let cpp_extra_args_acsl () = 
  let includePaths = 
    List.map (fun x -> "-I"^(!rootPath^"/"^(Filename.basename x))) (!Configuration.global_params.includePaths)
  in
  let macros = List.map (fun x -> "-D"^x) (!Configuration.global_params.macros) in
  let res = "\""^(String.concat " " includePaths)^(String.concat " " macros)^"\"" in 
  res

let source_files () = 
  let sourceFiles = List.map (fun x -> (!rootPath)^"/"^x) (!Configuration.global_params.sourceFiles) in
  (String.concat " " sourceFiles)

let kernel_boolean_args () =     
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  if (!Configuration.global_params.cc) then add_arg " -cc";
  if (!Configuration.global_params.cppGnuLike) then add_arg " -cpp-frama-c-compliant";
  if (not (!Configuration.global_params.framacStdlib)) then add_arg " -no-frama-c-stdlib";
  if (not (!Configuration.global_params.keepUnusedSpecifiedFunctions)) then add_arg " -remove-unused-specified-functions";
  if (not (!Configuration.global_params.keepUnusedTypes)) then add_arg " -remove-unused-types";
  if (!Configuration.global_params.aggressiveMerging) then add_arg " -aggressive-merging";
  if (!Configuration.global_params.origName) then add_arg " -orig-name";
  if (!Configuration.global_params.print) then add_arg " -print";
  if (not (!Configuration.global_params.annot)) then add_arg " -no-annot";
  if (!Configuration.global_params.keepComments) then add_arg " -keep-comments";

  add_arg " -kernel-warn-key annot-error=active";
  add_arg " -no-unicode";
  !args
  
let kernel_string_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  let not_empty s =
    not (String.equal s "")
  in
  if not_empty (!Configuration.global_params.cppCommand) then add_arg (" -cpp-command=\""^(!Configuration.global_params.cppCommand)^"\"");
  if not_empty (!Configuration.global_params.machdep) then add_arg (" -machdep=\""^(!Configuration.global_params.machdep)^"\"");
  if not_empty (!Configuration.global_params.kernelLog) then add_arg (" -kernel-log=\"eiufrwd:"^(!rootPath^"/"^(Filename.basename (!Configuration.global_params.kernelLog)))^"\"");
  (* 'key:value' args *)
  let generatedSpecCustom = String.concat "," (!Configuration.global_params.generatedSpecCustom) in
  if not_empty generatedSpecCustom then add_arg (" -generated-spec-custom=\""^generatedSpecCustom^"\"");
  !args

let metrics_boolean_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  add_arg " -metrics";
  if !Configuration.global_params.metricsByFunction then add_arg " -metrics-by-function";
  !args

let metrics_string_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  let not_empty s =
    not (String.equal s "")
  in
  if not_empty (!Configuration.global_params.metricsOutput) then add_arg (" -metrics-output=\""^(!rootPath)^"/"^(!Configuration.global_params.metricsOutput)^"\"") else add_arg (" -metrics-output=\""^(!rootPath)^"/"^"untitledMetrics.txt\"");
  !args 

let callgraph_string_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  let not_empty s =
    not (String.equal s "")
  in
  if not_empty (!Configuration.global_params.cg) then add_arg (" -cg=\""^(!rootPath^"/"^(!Configuration.global_params.cg))^"\"") else add_arg " -cg=\"untitledCallgraph.dot\"";
  (* 'key:value' args *)
  let cgRoots = String.concat "," (!Configuration.global_params.cgRoots) in
  if not_empty cgRoots then add_arg (" -cg-roots=\""^cgRoots^"\"");
  !args

let callgraph_boolean_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  if (!Configuration.global_params.cgServices) then add_arg " -cg-services" else add_arg " -cg-no-services";
  !args

let wp_boolean_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  (* add_arg " -wp"; *)
  if (!Configuration.global_params.wpRte) then add_arg " -wp-rte";
  if not (!Configuration.global_params.wpPruning) then add_arg " -wp-no-pruning";
  if (!Configuration.global_params.wpCheckMemoryModel) then add_arg " -wp-check-memory-model";
  if (!Configuration.global_params.wpVolatile) then add_arg " -wp-volatile";
  if (!Configuration.global_params.wpGen) then add_arg " -wp-gen";
  if (!Configuration.global_params.wpDump) then add_arg " -wp-dump";
  if (!Configuration.global_params.wpStatus) then add_arg " -wp-status";
  if (!Configuration.global_params.wpSmokeTests) then add_arg " -wp-smoke-tests";

  !args
let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

let send_request server_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send server_sock response_bytes 0 (Bytes.length response_bytes) [] in
  Printf.printf "Size of sent content : %d\n%!" sent

let readcontlen sock : string = 
  let contlenbuf = Bytes.create 1 in
  let res = ref "" in 
  let curr_char = ref "" in 
  while not (String.equal !curr_char "\n") do (* read the content length line character by character *)
  let data_len = Unix.read sock contlenbuf 0 1 in 
    ignore data_len;
    curr_char := (Bytes.to_string contlenbuf) ;
    res := !res ^ !curr_char;
  done;
  ignore (Unix.read sock contlenbuf 0 1); (* consume remaining "\r\n" from request header *) (* note : why 1 ? *)
  !res

let execute_command command = 
  (* Printf.printf "before wrapper sock\n%!"; *)
  let wrapper_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  (* Printf.printf "after wrapper sock\n%!"; *)
  Unix.bind wrapper_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, 8006));
  (* Printf.printf "after bind\n%!"; *)
  Unix.listen wrapper_sock 100;
  let ic = Unix.open_process_in command in
  ignore 
  (try 
    while true do
      Printf.printf "\t%s\n%!" (Stdlib.input_line ic);
    done;
  with End_of_file -> Printf.printf "\n%!";);
  (* Printf.printf "before accept\n%!"; *)
  let (plugin_sock, _) = Unix.accept wrapper_sock in
  let data_size = getnumber (readcontlen plugin_sock) in 
  let buffer = Bytes.make data_size '0' in
  let _req_data_len = Unix.read plugin_sock buffer 0 data_size in
  let request_str = (Bytes.to_string buffer) in
  (* Printf.printf "accept\n%!"; *)
  ignore (Unix.close_process_in ic);
  let _bytes_read = Unix.recv plugin_sock buffer 0 (Bytes.length buffer) [] in 
  (* Printf.printf "recv\n%!"; *)
  Unix.close plugin_sock;
  Unix.close wrapper_sock;
  request_str

let rq_handler json_string =
  
  let json = Json.load_string json_string in 
  let request = Lsp_types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  let id = request.id in
  try
    match curr_method with 
    | "initialize" -> 
      let req_json = (Lsp_types.RequestMessage.json_of_t request) in
      let temp = remove_newline (remove_quotes (Json.save_string (Json.field "rootPath" (Json.field "params" req_json)))) in
      rootPath := temp;
      Lsp_types.CONTENT (
        {|{
        "jsonrpc": "2.0",
        "id": 0,
        "result": {
          "capabilities": {
            "textDocumentSync": {
              "openClose": false,
              "change": 0,
              "save": {
                "includeText": false
              }
            },
            "definitionProvider": true,
            "declarationProvider": true,

            "diagnosticProvider": {
              "interFileDependencies": false,
              "workspaceDiagnostics": true
            },
            "workspace": {
              "workspaceFolders": {
                "supported": true,
                "changeNotifications": true
              }
            },
            "experimental": null
          },
          "serverInfo": {
            "name": "ACSL LSP",
            "version": "0.0.1"
          }
        }
      }|}
      );

    | "textDocument/definition" -> 
      Printf.printf "definition\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.DefinitionParams.t_of_json p
        | None -> Printf.printf "No definition params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let _file = remove_file_scheme (remove_newline (remove_quotes uri)) in
      let line = params.position.line in 
      let ch = params.position.character in
      
      (* let ic = Unix.open_process_in ("frama-c "^(String.concat " " sourceFiles)^" -cpp-extra-args=\""^(String.concat " " includePaths)^"\" -kernel-warn-key annot-error=active -acsl_lsp -find_def="^(Stdlib.string_of_int (id_to_int request.id))^":"^_file^":"^(Stdlib.string_of_int line)^":"^(Stdlib.string_of_int ch)) in *)
      let command = "frama-c "^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then -acsl_lsp -id=\""^(Stdlib.string_of_int (id_to_int request.id))^"\" -root_path=\""^(!rootPath)^"\" -find_def="^_file^":"^(Stdlib.string_of_int line)^":"^(Stdlib.string_of_int ch)^" -source_files=\""^(source_files ())^"\"" in
      Printf.printf "Command = %s\n%!" command;
      let data = execute_command command in
      Lsp_types.CONTENT data;
      
    | "textDocument/declaration" -> 
      Printf.printf "declaration\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.DeclarationParams.t_of_json p
        | None -> Printf.printf "No declaration params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let _file = remove_file_scheme (remove_newline (remove_quotes uri)) in
      let line = params.position.line in 
      let ch = params.position.character in
      
      (* let ic = Unix.open_process_in ("frama-c "^(String.concat " " sourceFiles)^" -cpp-extra-args=\""^(String.concat " " includePaths)^"\" -kernel-warn-key annot-error=active -acsl_lsp -find_def="^(Stdlib.string_of_int (id_to_int request.id))^":"^_file^":"^(Stdlib.string_of_int line)^":"^(Stdlib.string_of_int ch)) in *)
      let command = "frama-c"^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then -acsl_lsp -id=\""^(Stdlib.string_of_int (id_to_int request.id))^"\" -root_path=\""^(!rootPath)^"\" -find_decl="^_file^":"^(Stdlib.string_of_int line)^":"^(Stdlib.string_of_int ch)^" -source_files=\""^(source_files ())^"\"" in
      Printf.printf "Command = %s\n%!" command;
      let data = execute_command command in
      Lsp_types.CONTENT (data);

    (* | "completion" -> 
      Printf.printf "completion\n%!";
      Lsp_types.CONTENT (Completion.completion_items request); *)
    | "displayCIL" -> 
      Printf.printf "displayCIL\n%!";
      let file = match request.params with 
        | Some `List [f] -> remove_newline (remove_quotes (Json.save_string f))
        | _ -> Printf.printf "No params for displayCIL \n%!"; assert false
      in
      let command = "frama-c "^file^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then -acsl_lsp -display_cil -id=\""^(Stdlib.string_of_int (id_to_int request.id))^"\"" in
      Printf.printf "Command = %s\n%!" command;
      Lsp_types.CONTENT ((execute_command command));
    
    | "computeCG" -> 
      Printf.printf "computeCG\n%!";
      let file = match request.params with 
          | Some `List [f] -> remove_newline (remove_quotes (Json.save_string f))
          | _ -> Printf.printf "No params for computeCG \n%!"; assert false
        in
      let command = "frama-c "^file^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then"^(callgraph_string_args ())^(callgraph_boolean_args ())^" -then -acsl_lsp -id=\""^(Stdlib.string_of_int (id_to_int request.id))^"\" -compute_cg" in
      Printf.printf "Command = %s\n%!" command;
      Lsp_types.CONTENT ((execute_command command));

    | "showMetrics" -> 
      Printf.printf "metrics\n%!";
      let file = match request.params with 
          | Some `List [f] -> remove_newline (remove_quotes (Json.save_string f))
          | _ -> Printf.printf "No params for metrics \n%!"; assert false
        in
      let command = "frama-c "^file^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then"^(metrics_boolean_args ())^(metrics_string_args ())^" -then -acsl_lsp -id=\""^(Stdlib.string_of_int (id_to_int request.id))^"\" -show_metrics" in
      Printf.printf "Command = %s\n%!" command;
      Lsp_types.CONTENT ((execute_command command));
    
    | "showPOVC" -> (* show proof obligation of specific function *)
      Printf.printf "showPOVC, %d\n%!" (id_to_int request.id);
      let (file, line, ch) = match request.params with 
          | Some `List 
            [`List 
              [`String f; `Assoc [
                "line", `Int l;
                "character", `Int c;
              ]]] -> 
            (remove_newline (remove_quotes (f)), Stdlib.string_of_int(l), Stdlib.string_of_int(c))
          | _ -> Printf.printf "No params for showPOVC \n%!"; assert false
        in
      let files = 
        String.concat " " (match (String.ends_with ~suffix:".h" file) with
        | true -> file::(Utils.get_corr_cfile (!rootPath) file); 
        | false -> [file])
      in
      
      let command = "frama-c"^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then -acsl_lsp -id=\""^(Stdlib.string_of_int (id_to_int request.id))^"\" -root_path=\""^(!rootPath)^"\" -source_files=\""^files^"\" -acsl_wp=\""^file^"\" -show_povc=\""^file^":"^line^":"^ch^"\""^(wp_boolean_args ()) in
      Printf.printf "Command = %s\n%!" command;
      Lsp_types.CONTENT ((execute_command command));

    | "showAllPOVC" -> (* show proof obligations of entire file *)
      Printf.printf "showAllPOVC, %d\n%!" (id_to_int request.id);
      let file = match request.params with 
          | Some `List 
            [`List 
              [`String f]
            ] -> 
            (remove_newline (remove_quotes (f)))
          | _ -> Printf.printf "No params for showPOVC \n%!"; assert false
        in
      let command = "frama-c "^file^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then"^(wp_boolean_args ())^" -then -acsl_lsp -id=\""^(Stdlib.string_of_int (id_to_int request.id))^"\" -show_povc_all" in
      Printf.printf "Command = %s\n%!" command;
      Lsp_types.CONTENT ((execute_command command));

    | "shutdown" -> receivedShutdown := true; 
      Lsp_types.CONTENT (Json.save_string (Shutdown.shutdown request));
    | _ -> 
      Lsp_types.CONTENT (Json.save_string `Null)
  with exn ->  
    Printf.printf "Request error \n%!";
    Printf.printf "Backtrace : %s\n" (Printexc.get_backtrace ());
    Lsp_types.CONTENT (Json.save_string (make_error (Printexc.to_string (exn)) (id_to_int id)))


let notif_handler json_string server_sock =
  let json = Json.load_string json_string in 
  let notif = Lsp_types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    Printf.printf "initialized\n%!";
    send_request server_sock (Json.save_string Configuration.request_configurations);
    Lsp_types.CONTENT (Json.save_string (
      RegisterCapability.registerCapabilityRequest 
      (RegisterCapability.registrationParams 
        ([RegisterCapability.registration "workspace/didChangeConfiguration"])
      );
    ))

  (* | "didOpen" ->
    Printf.printf "didOpen\n%!";
    Lsp_types.EMPTY (DidOpen.handle notif server_sock); *)
  | "textDocument/didSave" ->
    Printf.printf "didSave\n%!";
    let params = match notif.params with 
      | Some p -> Lsp_types.DidSaveTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in 
    let _file = remove_file_scheme (remove_newline (remove_quotes uri)) in
    (* let ic = Unix.open_process_in (Filename.quote_command "frama-c" ["-acsl_lsp"; ("-did_save="^_file)]) in *)
    (* let ic = Unix.open_process_in ("frama-c -acsl_lsp -did_save=" ^ _file) in *)
    (* let ic = Unix.open_process_in ("frama-c " ^ _file ^ " -kernel-warn-key annot-error=active -kernel-warn-key cmdline=active -acsl_lsp -did_save") in *)
    let command = "frama-c"^(cpp_extra_args ())^(kernel_boolean_args ())^(kernel_string_args ())^" -then -wp -wp-print -acsl_lsp -did_save=" ^ _file in
    Printf.printf "Command = %s\n%!" command;
    (* let ic = Unix.open_process_in ("frama-c " ^ _file ^ " -acsl_lsp -did_save") in *)
    (* Printf.printf "open in\n%!"; *)
    (* let _ = Unix.wait () in *)
    Lsp_types.CONTENT ((execute_command command));

  | "workspace/didChangeConfiguration" ->
    Printf.printf "didChangeConfiguration\n%!";
    Lsp_types.CONTENT (Json.save_string (Configuration.request_configurations));

  | "exit" -> if !receivedShutdown then Unix._exit 0 else Unix._exit 1
  | _ -> 
      Lsp_types.EMPTY ()



let result_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Lsp_types.ResponseMessage.t_of_json json in 
  let result = match request.result with 
    | Some r -> r
    | None -> Printf.printf "No result \n%!"; assert false
  in 

  let id = request.id in
  match id with 
  | Lsp_types.Str "ask_configs" -> (* if the result is request_configurations *)
    Configuration.save_configs (result);
    Lsp_types.EMPTY ();
  | _ -> 
    Lsp_types.EMPTY ()

(* todo : implement client error handling with different error codes *)
let error_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Lsp_types.ResponseMessage.t_of_json json in 
  let error = 
    match request.error with 
    | Some err -> err 
    | None -> Printf.printf "No error \n%!"; assert false
  in 
  Lsp_types.ResponseError.json_of_t (error)

let handle (json_string : string) server_sock : Lsp_types.lsp_result = 
  (* if !receivedShutdown then 
    Lsp_types.CONTENT (Shutdown.shutdown_error (Lsp_types.RequestMessage.t_of_json (Json.load_string json_string))) else  *)
  if (is_result json_string) then (* todo : how to do this with a match with *)
    begin
      Printf.printf "result_handler\n%!";
      result_handler json_string 
    end
  else if (is_error json_string) then 
    begin
      Printf.printf "error_handler\n%!";
      Lsp_types.CONTENT (Json.save_string (error_handler json_string))
    end
  else if (is_notif json_string) then 
    begin
      Printf.printf "notif_handler\n%!";
      (* Printf.printf "Received from client : %s\n%!" json_string; *)
      notif_handler json_string server_sock
    end
  else if (is_request json_string) then 
    begin
      Printf.printf "rq_handler\n%!";
      rq_handler json_string
    end
  else 
    raise (Failure "Unknown request")
