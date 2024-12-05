
(* "completionProvider": {
              "triggerCharacters": [],
              "allCommitCharacters": [],
              "resolveProvider": false,
              "completionItem": {
                "labelDetailsSupport": false
              }
            }, *)
let registerCapabilityRequest json = 
  Lsp_types.RequestMessage.json_of_t (Lsp_types.RequestMessage.create
    ~jsonrpc:"2.0"
    ~id:(Lsp_types.Str "register_capability") (* give proper id *)
    ~method_:"client/registerCapability"
    ~params:json
    ())

let registration method_ = 
    Lsp_types.Registration.create
      ~id:"registration"
      ~method_:method_
      ()

let registrationParams registrations = 
  Lsp_types.RegistrationParams.json_of_t (
    Lsp_types.RegistrationParams.create
      ~registrations:registrations
      ()
  )

let shutdown (req : Lsp_types.RequestMessage.t) : Json.json = 
  Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())

let shutdown_error (req : Lsp_types.RequestMessage.t) : Json.json = 
  Lsp_types.ResponseMessage.json_of_t (
    (Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0" 
    ~id:req.id 
    ~error:(Lsp_types.ResponseError.create 
      ~code:(-32600)
      ~message:"Invalid request received after shutdown"
    ()) 
    ) 
  ())
  
let receivedShutdown = ref false
let rootPath = ref ""


let is_result json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "result") fields)
    | _ -> Lsp.Self.debug ~level:3 "no result\n%!"; false
  with
  | Json.Error _ -> false

let is_error json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "error") fields)
    | _ -> Lsp.Self.debug ~level:3 "no error\n%!"; false

  with
  | Json.Error _ -> false

let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      not (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Lsp.Self.debug ~level:3 "no notif\n%!"; false

  with
  | Json.Error _ -> false

let is_request json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Lsp.Self.debug ~level:3 "no request\n%!"; false

  with
  | Json.Error _ -> false
  
let debug () = 
  Stdlib.string_of_int !Configuration.global_params.acslLsp

let wp_diags () = 
  !Configuration.global_params.diagnosticsWp

let cpp_extra_args () = 
  let includePaths = 
    List.map (fun x -> " -I"^(!rootPath^"/"^(x))) (!Configuration.global_params.includePaths)
  in
  let macros = List.map (fun x -> " -D"^x) (!Configuration.global_params.macros) in
  let res = " -cpp-extra-args=\" -CC "^(String.concat " " includePaths)^(String.concat " " macros)^"\"" in 
  res

let cpp_extra_args_acsl () = 
  let includePaths = 
    List.map (fun x -> " -I"^(!rootPath^"/"^(x))) (!Configuration.global_params.includePaths)
  in
  let macros = List.map (fun x -> " -D"^x) (!Configuration.global_params.macros) in
  let res = "\""^(String.concat " " includePaths)^(String.concat " " macros)^"\"" in 
  res

let source_files () = 
  let sourceFiles = List.map (fun x -> (!rootPath)^"/"^x) (!Configuration.global_params.sourceFiles) in
  (String.concat " " sourceFiles)
  
let kernel_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  let not_empty s =
    not (String.equal s "")
  in
  if not_empty (!Configuration.global_params.machdep) then add_arg (" -machdep=\""^(!Configuration.global_params.machdep)^"\"");
  let generatedSpecCustom = String.concat "," (!Configuration.global_params.generatedSpecCustom) in
  if not_empty generatedSpecCustom then add_arg (" -generated-spec-custom=\""^generatedSpecCustom^"\"");

  if (not (!Configuration.global_params.keepUnusedSpecifiedFunctions)) then add_arg " -remove-unused-specified-functions";
  if (!Configuration.global_params.aggressiveMerging) then add_arg " -aggressive-merging";

  add_arg " -kernel-warn-key annot-error=active";
  add_arg " -no-unicode";
  !args

let global_metrics_args () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  let not_empty s =
    not (String.equal s "")
  in
  add_arg " -metrics";
  add_arg " -metrics-by-function";
  if not_empty (!Configuration.global_params.metricsOutput) then 
      add_arg (" -metrics-output=\""^(!rootPath)^"/"^(Filename.remove_extension !Configuration.global_params.metricsOutput)^".txt\"") 
  else add_arg (" -metrics-output=\"project_metrics.txt\"");
  !args 

let callgraph_args file () = 
  let args = ref "" in
  let add_arg arg = 
    args := !args^arg 
  in
  let not_empty s =
    not (String.equal s "")
  in
  if not_empty (!Configuration.global_params.cgOutput) then add_arg (" -cg=\""^(!rootPath^"/"^(!Configuration.global_params.cgOutput))^".dot\"") else add_arg (" -cg=\""^file^".dot\"");
  (* 'key:value' args *)
  let cgRoots = String.concat "," (!Configuration.global_params.cgRoots) in
  if not_empty cgRoots then add_arg (" -cg-roots=\""^cgRoots^"\"");

  if (!Configuration.global_params.cgServices) then add_arg " -cg-services" else add_arg " -cg-no-services";

  !args

let get_cg_output_file file () = 
  let not_empty s =
    not (String.equal s "")
  in
  if not_empty (!Configuration.global_params.cgOutput) then ((!rootPath^"/"^(!Configuration.global_params.cgOutput))) else file

let wp_args () = 
  let args = ref "" in
  let add_arg arg = args := !args^arg in
  let not_empty s = not (String.equal s "") in
  add_arg " -wp";
  add_arg " -wp-gen";
  if (!Configuration.global_params.wpRte) then add_arg " -wp-rte";
  if not (!Configuration.global_params.wpPruning) then add_arg " -wp-no-pruning";
  if (!Configuration.global_params.wpCheckMemoryModel) then add_arg " -wp-check-memory-model";
  if (!Configuration.global_params.wpVolatile) then add_arg " -wp-volatile";
  if not_empty (!Configuration.global_params.wpProver) then add_arg (" -wp-prover=\""^(!Configuration.global_params.wpProver)^"\"");
  if not_empty (!Configuration.global_params.wpSession) then add_arg (" -wp-session=\""^(!Configuration.global_params.wpSession)^"\"");
  add_arg (" -wp-timeout=\""^Stdlib.string_of_int(!Configuration.global_params.wpTimeout)^"\"");

  !args
let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

let send_request server_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send server_sock response_bytes 0 (Bytes.length response_bytes) [] in
  Lsp.Self.debug ~level:4 "Size of sent content : %d\n%!" sent

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

let execute_command command didSave ?id () = 
  let had_errors = ref false in 
  let msg = ref "" in
  let response_id : Lsp_types.id_ = match id with 
    | Some id -> id
    | None -> (Lsp_types.Str "frama_c_error")
  in
  (* Lsp.Self.debug ~level:4 "before wrapper sock\n%!"; *)
  let wrapper_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  (* Lsp.Self.debug ~level:4 "after wrapper sock\n%!"; *)
  Unix.bind wrapper_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, 8006));
  (* Lsp.Self.debug ~level:4 "after bind\n%!"; *)
  Unix.listen wrapper_sock 100;
  let ic = Unix.open_process_in command in
  ignore 
  (try 
    while true do
      msg := Stdlib.input_line ic;
      Lsp.Self.debug "\t%s\n%!" (!msg);
      if (Utils.contains !msg ~suffix:"syntax error" 
        || Utils.contains !msg ~suffix:"There were parsing errors in"
        || Utils.contains !msg ~suffix:"User Error"
        || Utils.contains !msg ~suffix:"invalid user input"
        || Utils.contains !msg ~suffix:"Invalid symbol"
        || Utils.contains !msg ~suffix:"before or at token"
      ) then had_errors := true;
    done;
  with End_of_file -> Lsp.Self.debug ~level:4 "\n%!";);
  (* Lsp.Self.debug ~level:4 "before accept\n%!"; *)
  match !had_errors && (not didSave) with 
  | true -> 
    Lsp.Self.debug ~level:2 "Error while executing frama-c command\n%!";
    (* Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac)); *)
    let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t 
      (Lsp_types.ResponseMessage.create 
        ~jsonrpc:"2.0" 
        ~id:response_id
        ~error: (Lsp_types.ResponseError.create
          ~code:(-32603)
          ~message:!msg
          ()
        )
        ()
      )) in
    Unix.close wrapper_sock;
    data
  | false ->
    let (plugin_sock, _) = Unix.accept wrapper_sock in
    let data_size = getnumber (readcontlen plugin_sock) in 
    let buffer = Bytes.make data_size '0' in
    let _req_data_len = Unix.read plugin_sock buffer 0 data_size in
    let request_str = (Bytes.to_string buffer) in
    (* Lsp.Self.debug ~level:4 "accept\n%!"; *)
    ignore (Unix.close_process_in ic);
    let _bytes_read = Unix.recv plugin_sock buffer 0 (Bytes.length buffer) [] in 
    (* Lsp.Self.debug ~level:4 "recv\n%!"; *)
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
      Lsp.Self.debug ~level:4 "initialize\n%!";
      let req_json = (Lsp_types.RequestMessage.json_of_t request) in
      let temp = Utils.remove_newline (Utils.remove_quotes (Json.save_string (Json.field "rootPath" (Json.field "params" req_json)))) in
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
              "save": { "includeText": false }
            },
            "definitionProvider": true,
            "declarationProvider": true,
            
            "diagnosticProvider": {
              "interFileDependencies": false,
              "workspaceDiagnostics": true
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
      Lsp.Self.debug ~level:4 "definition\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.DefinitionParams.t_of_json p
        | None -> Lsp.Self.debug ~level:3 "No definition params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let src_file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
      let line = params.position.line in 
      let ch = params.position.character in
      let files_to_parse = 
        match source_files () with 
          | "" -> String.concat " " (Utils.get_workspace_files !rootPath)
          | _ -> (source_files ())
        in
      (* let command = "frama-c "^files_to_parse^" "^(cpp_extra_args ())^(kernel_args ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\" -lsp-root-path=\""^(!rootPath)^"\" -lsp-definition="^_file^":"^(Stdlib.string_of_int line)^":"^(Stdlib.string_of_int ch) in *)
      let command = Printf.sprintf "frama-c %s %s %s -then -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-id=\"%s\" -lsp-root-path=\"%s\" -lsp-definition=%s:%s:%s"
      files_to_parse (cpp_extra_args ()) (kernel_args ())
      (debug ()) (Stdlib.string_of_int (Utils.id_to_int request.id)) (!rootPath)
      src_file (Stdlib.string_of_int line) (Stdlib.string_of_int ch) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      let data = execute_command command false ~id:request.id () in
      Lsp_types.CONTENT data;
      
    | "textDocument/declaration" -> 
      Lsp.Self.debug ~level:4 "declaration\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.DeclarationParams.t_of_json p
        | None -> Lsp.Self.debug ~level:3 "No declaration params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let _file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
      let line = params.position.line in 
      let ch = params.position.character in

      let files_to_parse = 
        match source_files () with 
          | "" -> String.concat " " (Utils.get_workspace_files !rootPath)
          | _ -> (source_files ())
        in
      
      let command = "frama-c "^files_to_parse^" "^(cpp_extra_args ())^(kernel_args ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\" -lsp-root-path=\""^(!rootPath)^"\" -lsp-declaration="^_file^":"^(Stdlib.string_of_int line)^":"^(Stdlib.string_of_int ch) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      let data = execute_command command false ~id:request.id () in
      Lsp_types.CONTENT (data);
(*
    | "displayCIL" -> 
      Lsp.Self.debug ~level:4 "displayCIL\n%!";
      let file = match request.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for displayCIL \n%!"; assert false
      in
      (* let command = "frama-c "^file^(cpp_extra_args ())^(kernel_args ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-display-cil -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\"" in *)
      let command = Printf.sprintf "frama-c %s %s %s -then -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-display-cil -lsp-id=\"%s\""
      file (cpp_extra_args ()) (kernel_args ())
      (debug ()) (Stdlib.string_of_int (Utils.id_to_int request.id)) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      Lsp_types.CONTENT (execute_command command false ~id:request.id ());
*)    
    | "showPOVC" -> (* show proof obligation of specific function *)
      Lsp.Self.debug ~level:4 "showPOVC, %d\n%!" (Utils.id_to_int request.id);
      let (file, line, ch) = match request.params with 
          | Some `List 
            [`List 
              [`String f; `Assoc [
                "line", `Int l;
                "character", `Int c;
              ]]] -> 
            (Utils.remove_newline (Utils.remove_quotes (f)), Stdlib.string_of_int(l), Stdlib.string_of_int(c))
          | _ -> Lsp.Self.debug ~level:3 "No params for showPOVC \n%!"; assert false
        in
      (* additionnal source files if the parsed file is a header file *)
      let files = 
        String.concat " " (match (String.ends_with ~suffix:".h" file) with
        | true -> file::(Utils.get_corr_cfile (!rootPath) file); 
        | false -> [file])
      in
      
      let command = "frama-c "^files^(cpp_extra_args ())^(kernel_args ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\" -lsp-root-path=\""^(!rootPath)^"\" -lsp-show-povc=\""^file^":"^line^":"^ch^"\""^(wp_args ()) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      Lsp_types.CONTENT (execute_command command false ~id:request.id ());

    | "textDocument/completion" -> 
      Lsp.Self.debug ~level:4 "completion\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.CompletionParams.t_of_json p
        | None -> Lsp.Self.debug ~level:3 "No completion params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
      let line = Stdlib.string_of_int params.position.line in 
      let ch = Stdlib.string_of_int params.position.character in 
      let command = "frama-c -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\" -lsp-completion=\""^file^":"^line^":"^ch^"\"" in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      Lsp_types.CONTENT (execute_command command false ~id:request.id ());

    | "shutdown" -> receivedShutdown := true; 
      Lsp_types.CONTENT (Json.save_string (shutdown request));
    | _ -> 
      Lsp_types.CONTENT (Json.save_string `Null)
  with exn ->  
    Lsp.Self.debug ~level:3 "Request error \n%!";
    Lsp.Self.debug ~level:3 "Backtrace : %s\n" (Printexc.get_backtrace ());
    Lsp_types.CONTENT (Json.save_string (Utils.make_error (Printexc.to_string (exn)) (Utils.id_to_int id)))


let notif_handler json_string server_sock =
  let json = Json.load_string json_string in 
  let notif = Lsp_types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    Lsp.Self.debug ~level:4 "initialized\n%!";
    send_request server_sock (Json.save_string Configuration.request_configurations);
    Lsp_types.CONTENT (Json.save_string (
      registerCapabilityRequest 
      (registrationParams 
        ([registration "workspace/didChangeConfiguration"])
      );
    ))

  (* | "textDocument/didOpen" ->
    Lsp.Self.debug ~level:4 "didOpen\n%!";
    let params = match notif.params with 
      | Some p -> Lsp_types.DidOpenTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in 
    let _file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    let command = "frama-c"^(cpp_extra_args ())^(kernel_args ())^" -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-did-open=" ^ _file in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
    Lsp_types.CONTENT ((execute_command command false)); *)

  | "textDocument/didClose" ->
    Lsp.Self.debug ~level:4 "didClose\n%!";
    let params = match notif.params with 
      | Some p -> Lsp_types.DidCloseTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in 
    let _file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    let command = "frama-c -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-did-close=" ^ _file in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
    Lsp_types.CONTENT (execute_command command false ());

  | "textDocument/didSave" ->
    Lsp.Self.debug ~level:4 "didSave\n%!";
    let params = match notif.params with 
      | Some p -> Lsp_types.DidSaveTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in 
    let file_name = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    if String.ends_with ~suffix:".c" file_name then
      begin
      let command = "frama-c"^(cpp_extra_args ())^(kernel_args ())^" -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-did-save=" ^ file_names in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      Lsp_types.CONTENT (execute_command command true ());
      end
    else Lsp_types.EMPTY ()

  | "showGlobalMetrics" -> 
    Lsp.Self.debug ~level:4 "global metrics\n%!";
    let project_filename = if not (String.equal (!Configuration.global_params.metricsOutput) "") then (Filename.remove_extension !Configuration.global_params.metricsOutput) else "project_metrics" in
    let command = Printf.sprintf "frama-c %s %s %s -then %s -then -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-metrics=\"%s/%s\""
    (source_files ()) (cpp_extra_args ()) (kernel_args ()) (global_metrics_args ()) (debug ()) (!rootPath) project_filename in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
    Lsp_types.CONTENT (execute_command command false ());

  | "displayCIL" -> 
      Lsp.Self.debug ~level:4 "displayCIL\n%!";
      let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for displayCIL \n%!"; assert false
      in
      let command = Printf.sprintf "frama-c %s %s %s -then -print -no-unicode -ocode \"%s_fc.c\" -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-display-cil=\"%s\""
      file (cpp_extra_args ()) (kernel_args ())
      (Filename.remove_extension file)
      (debug ())
      (Filename.remove_extension file) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      Lsp_types.CONTENT (execute_command command false ());

  | "displayCIL_noannot" -> 
        Lsp.Self.debug ~level:4 "displayCIL_noannot\n%!";
        let file = match notif.params with 
          | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
          | _ -> Lsp.Self.debug ~level:3 "No params for displayCIL \n%!"; assert false
        in
        let command = Printf.sprintf "frama-c %s %s %s -then -print -no-unicode -ocode \"%s_fc.c\" -no-annot -keep-comments -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-display-cil=\"%s\""
        file (cpp_extra_args ()) (kernel_args ())
        (Filename.remove_extension file)
        (debug ())
        (Filename.remove_extension file) in
        Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
        Lsp_types.CONTENT (execute_command command false ());

  | "showLocalMetrics" -> 
    Lsp.Self.debug ~level:4 "local metrics\n%!";
    let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for metrics \n%!"; assert false
    in
    let command = Printf.sprintf "frama-c %s %s %s -then -metrics -metrics-by-function -metrics-output=\"%s.txt\" -then -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-metrics=\"%s\""
    file (cpp_extra_args ()) (kernel_args ())
    (Filename.remove_extension file) (debug ()) (Filename.remove_extension file) in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
    Lsp_types.CONTENT (execute_command command false ());
  
  | "computeCG" -> 
    Lsp.Self.debug ~level:4 "computeCG\n%!";
    let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for computeCG \n%!"; assert false
    in
    let command = "frama-c "^file^(cpp_extra_args ())^(kernel_args ())^" -then"^(callgraph_args (Filename.remove_extension file) ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-compute-cg=\""^(get_cg_output_file (Filename.remove_extension file) ())^"\"" in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      Lsp_types.CONTENT (execute_command command false ());

  | "workspace/didChangeConfiguration" ->
    Lsp.Self.debug ~level:4 "didChangeConfiguration\n%!";
    Lsp_types.CONTENT (Json.save_string (Configuration.request_configurations));

  | "exit" -> if !receivedShutdown then Unix._exit 0 else Unix._exit 1
  | _ -> 
      Lsp_types.EMPTY ()



let result_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Lsp_types.ResponseMessage.t_of_json json in 
  let result = match request.result with 
    | Some r -> r
    | None -> Lsp.Self.debug ~level:3 "No result \n%!"; assert false
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
    | None -> Lsp.Self.debug ~level:3 "No error \n%!"; assert false
  in 
  Lsp_types.ResponseError.json_of_t (error)

let handle (json_string : string) server_sock : Lsp_types.lsp_result = 
  (* if !receivedShutdown then 
    Lsp_types.CONTENT (Shutdown.shutdown_error (Lsp_types.RequestMessage.t_of_json (Json.load_string json_string))) else  *)
  if (is_result json_string) then (* todo : how to do this with a match with *)
    begin
      Lsp.Self.debug ~level:4 "result_handler\n%!";
      result_handler json_string 
    end
  else if (is_error json_string) then 
    begin
      Lsp.Self.debug ~level:4 "error_handler\n%!";
      Lsp_types.CONTENT (Json.save_string (error_handler json_string))
    end
  else if (is_notif json_string) then 
    begin
      Lsp.Self.debug ~level:4 "notif_handler\n%!";
      (* Lsp.Self.debug ~level: "Received from client : %s\n%!" json_string; *)
      notif_handler json_string server_sock
    end
  else if (is_request json_string) then 
    begin
      Lsp.Self.debug ~level:4 "rq_handler\n%!";
      rq_handler json_string
    end
  else 
    raise (Failure "Unknown request")
