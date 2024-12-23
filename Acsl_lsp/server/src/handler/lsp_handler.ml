

(* "completionProvider": {
              "triggerCharacters": [],
              "allCommitCharacters": [],
              "resolveProvider": false,
              "completionItem": {
                "labelDetailsSupport": false
              }
            }, *)


let rootPath = ref ""

type lsp_feature = 
  | DidSave_feature
  | DidClose_feature
  | FindDefinition_feature of (int * string * int * int)
  | FindDeclaration_feature
  | ComputeCIL_feature
  | ComputeCallGraph_feature
  | ComputeMetrics_feature
  | ComputeProofObligation_feature


module KernelOpt = struct
type t = {
  include_paths : string list;
  macros : string list;
  cpp_extra_args : string;
  machdep : string;
  generated_spec_custom : string list;
  keep_unused_specified_functions : bool;
  aggressive_merging : bool;
  kernel_warn_key : string;
  no_unicode : bool;
  inline_calls : string;
  remove_inlines : string
  }
let create () =
  {
    include_paths = !Configuration.global_params.includePaths;
    macros = !Configuration.global_params.macros;
    cpp_extra_args = "-CC";
    machdep = !Configuration.global_params.machdep;
    generated_spec_custom = !Configuration.global_params.generatedSpecCustom;
    keep_unused_specified_functions = !Configuration.global_params.keepUnusedSpecifiedFunctions;
    aggressive_merging = !Configuration.global_params.aggressiveMerging;
    kernel_warn_key = "annot-error=active,too-large-array=active";
    no_unicode = true;
    inline_calls = "@inline";
    remove_inlines = "@inline"
  }
let string_of_t (options : t) : string =
  let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
  let option_if_true b opt = if b then (opt) else "" in
  let include_paths_opt = List.map (fun x -> " -I"^(!rootPath^"/"^(x))) options.include_paths in
  let macros_opt = List.map (fun x -> " -D"^x) options.macros in
  let cpp_extra_args_opt = "-cpp-extra-args=\" "^(options.cpp_extra_args)^" "^(String.concat " " include_paths_opt)^(String.concat " " macros_opt)^"\"" in
  let machdep_opt = (option_if_not_empty_string options.machdep "-machdep ") in
  let generated_spec_custom_opt = option_if_not_empty_string (String.concat "," options.generated_spec_custom) "-generated-spec-custom=" in
  let remove_unused_specified_functions_opt = option_if_true options.keep_unused_specified_functions "-remove-unused-specified-functions " in
  let aggressive_merging_opt = option_if_true options.aggressive_merging "-aggressive-merging " in
  let kernel_warn_key_opt = option_if_not_empty_string options.kernel_warn_key "-kernel-warn-key " in
  let no_unicode_opt = option_if_true options.no_unicode "-no-unicode" in
  let inline_calls_opt = option_if_not_empty_string options.inline_calls "-inline-calls " in
  let remove_inlines_opt = option_if_not_empty_string options.remove_inlines "-remove-inlined " in
  Printf.sprintf "%s %s %s %s %s %s %s %s %s"
  cpp_extra_args_opt machdep_opt generated_spec_custom_opt remove_unused_specified_functions_opt aggressive_merging_opt kernel_warn_key_opt no_unicode_opt inline_calls_opt remove_inlines_opt
end

module WpOpt = struct
  type t = {
    wp: bool;
    wp_rte: bool;
    wp_prop: string list;
    wp_fct: string list;
    wp_gen: bool;
    wp_pruning: bool;
    wp_check_memory_model: bool;
    wp_no_volatile: bool;
    wp_prover: string;
    wp_timeout: int;
    wp_session: string
  }
  let create () = {
    wp = true;
    wp_rte = !Configuration.global_params.wpRte;
    wp_prop = ["@assigns"];
    wp_fct = [];
    wp_gen = true;
    wp_pruning = !Configuration.global_params.wpPruning;
    wp_check_memory_model = !Configuration.global_params.wpCheckMemoryModel;
    wp_no_volatile = !Configuration.global_params.wpVolatile;
    wp_prover = !Configuration.global_params.wpProver;
    wp_timeout = !Configuration.global_params.wpTimeout;
    wp_session = !Configuration.global_params.wpSession
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let wp_opt = option_if_true options.wp "-wp" in
    let wp_prop_opt = option_if_not_empty_string (String.concat "," options.wp_prop) "-wp-prop " in
    let wp_fct_opt = option_if_not_empty_string (String.concat "," options.wp_fct) "-wp-fct " in
    let wp_gen_opt = option_if_true options.wp "-wp-gen" in
    let wp_rte_opt = option_if_true options.wp_rte "-wp-rte" in
    let wp_pruning_opt = option_if_true options.wp_pruning "-wp-no-pruning" in
    let wp_check_memory_model_opt = option_if_true options.wp_check_memory_model "-wp-model \"Typed+var+int+float\" -wp-check-memory-model" in
    let wp_no_volatile_opt = option_if_true options.wp_no_volatile "-wp-no-volatile" in
    let wp_prover_opt = option_if_not_empty_string options.wp_prover "-wp-prover " in
    let wp_timeout_opt = Printf.sprintf "-wp-timeout %d" options.wp_timeout in
    let wp_session_opt = option_if_not_empty_string options.wp_session "-wp-session " in
    Printf.sprintf "%s %s %s %s %s %s %s %s %s %s %s" wp_opt wp_prop_opt wp_fct_opt wp_gen_opt wp_rte_opt wp_pruning_opt wp_check_memory_model_opt wp_no_volatile_opt wp_prover_opt wp_timeout_opt wp_session_opt
end

module MetacslOpt = struct
  type t = {
    meta : bool;
    meta_warn_key : string;
    meta_checks: bool;
    meta_no_simpl: bool;
    meta_no_check_ext: bool;
    meta_number_assertions: bool;
    meta_check_callee_assigns: string list
  }
  let create () = {
    meta = true;
    meta_warn_key = "unknown-func=active";
    meta_checks = true;
    meta_no_simpl = true;
    meta_no_check_ext = true;
    meta_number_assertions = true;
    meta_check_callee_assigns = []
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let meta_opt = option_if_true options.meta "-meta" in
    let meta_warn_key_opt = option_if_not_empty_string options.meta_warn_key "-meta-warn-key " in
    let meta_checks_opt = option_if_true options.meta_checks "-meta-checks" in
    let meta_no_simpl_opt = option_if_true  options.meta_no_simpl "-meta-no-simpl" in
    let meta_no_check_ext_opt = option_if_true options.meta_no_check_ext "-meta-no-check-ext" in
    let meta_number_assertions_opt = option_if_true options.meta_number_assertions "-meta-number-assertions" in
    let meta_check_callee_assigns_opt = option_if_not_empty_string (String.concat "," options.meta_check_callee_assigns) "-meta-check-callee-assigns " in
    Printf.sprintf "%s %s %s %s %s %s %s %s" meta_opt meta_warn_key_opt meta_warn_key_opt meta_checks_opt meta_no_simpl_opt meta_no_check_ext_opt meta_number_assertions_opt meta_check_callee_assigns_opt
end

module UncastOpt = struct
  type t = {
    uncast: bool;
    uncast_endianness: string;
    uncast_lshift_as_mul: bool;
    uncast_rshift_as_div: bool
  }
  let create () = {
    uncast = true;
    uncast_endianness = "little";
    uncast_lshift_as_mul = true;
    uncast_rshift_as_div = true
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let uncast_opt = option_if_true options.uncast "-uncast " in
    let uncast_endianness_opt = option_if_not_empty_string options.uncast_endianness "-uncast-endianness " in
    let uncast_lshift_as_mul_opt = option_if_true options.uncast_lshift_as_mul "-uncast-lshift-as-mul " in
    let uncast_rshift_as_div_opt = option_if_true options.uncast_rshift_as_div "-uncast-rshift-as-div " in
    Printf.sprintf "%s %s %s %s" uncast_opt uncast_endianness_opt uncast_lshift_as_mul_opt uncast_rshift_as_div_opt
end


module LspOpt = struct
  type t = lsp_feature
  let create (feature : t) = feature
  let string_of_t (feature : t) : string =
    match feature with
    | DidSave_feature -> "-lsp-did-save"
    | DidClose_feature -> "-lsp-did-close"
    | FindDefinition_feature(id, file, line, column) -> Printf.sprintf "-lsp-id=\"%d\" -lsp-definition=%s:%d:%d -lsp-find-definition" id file line column
    | FindDeclaration_feature -> "-lsp-find-declaration"
    | ComputeCIL_feature -> "-lsp-display-cil"
    | ComputeCallGraph_feature -> "-lsp-compute-cg"
    | ComputeMetrics_feature -> "-lsp-metrics"
    | ComputeProofObligation_feature -> "-lsp-show-povc"
end


module Command = struct
  type t = {
  verbose: int;
  files : string list;
  kernel : KernelOpt.t;
  wp : WpOpt.t option;
  metacsl : MetacslOpt.t option;
  uncast : UncastOpt.t option;
  lsp : LspOpt.t;
  }
  let create ~kernel ~files ?wp ?metacsl ?uncast ~lsp () : t = {
    verbose = !Configuration.global_params.acslLsp;
    files = files;
    kernel = kernel;
    wp = wp;
    metacsl = metacsl;
    uncast = uncast;
    lsp = lsp
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (s ^ " " ^ opt) else "" in
    let file_names = String.concat " " options.files in
    let debug_level = Stdlib.string_of_int options.verbose in
    let common_opt = Printf.sprintf "frama-c -lsp -lsp-no-cmdline -lsp-debug=%s %s" debug_level file_names in
    let kernel_opt = KernelOpt.string_of_t options.kernel in
    let uncast_opt = match options.uncast with None -> "" | Some u -> option_if_not_empty_string (UncastOpt.string_of_t u) "-then-last" in
    let wp_opt = match options.wp with None -> "" | Some w -> option_if_not_empty_string (WpOpt.string_of_t w) "" in
    let metacsl_opt = match options.metacsl with None -> "" | Some m -> option_if_not_empty_string (MetacslOpt.string_of_t m) "-then-last" in
    let lsp_opt = option_if_not_empty_string (LspOpt.string_of_t options.lsp) "" in
    Printf.sprintf "%s %s %s %s %s -then %s ; echo \"FRAMA-C EXIT CODE: $?\"" common_opt kernel_opt uncast_opt metacsl_opt wp_opt lsp_opt
end


let registerCapabilityRequest json =
  let msg = Lsp_types.RequestMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Str "register_capability") ~method_:"client/registerCapability" ~params:json () in
  Lsp_types.RequestMessage.json_of_t (msg)

let registration method_ = Lsp_types.Registration.create ~id:"registration" ~method_:method_ ()

let registrationParams registrations =
  let msg = Lsp_types.RegistrationParams.create ~registrations:registrations () in
  Lsp_types.RegistrationParams.json_of_t (msg)

let shutdown (req : Lsp_types.RequestMessage.t) : Json.json =
  Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())

let shutdown_error (req : Lsp_types.RequestMessage.t) : Json.json = 
  let error_msg = Lsp_types.ResponseError.create ~code:(-32600) ~message:"Invalid request received after shutdown" () in
  let msg = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~error:(error_msg) () in
  Lsp_types.ResponseMessage.json_of_t (msg)
  
let receivedShutdown = ref false
  
let debug () = Stdlib.string_of_int !Configuration.global_params.acslLsp

let wp_diags () = !Configuration.global_params.diagnosticsWp

let cpp_extra_args () = 
  let includePaths = List.map (fun x -> " -I"^(!rootPath^"/"^(x))) (!Configuration.global_params.includePaths) in
  let macros = List.map (fun x -> " -D"^x) (!Configuration.global_params.macros) in
  let res = " -cpp-extra-args=\" -CC "^(String.concat " " includePaths)^(String.concat " " macros)^"\"" in
  res

let cpp_extra_args_acsl () = 
  let includePaths = List.map (fun x -> " -I"^(!rootPath^"/"^(x))) (!Configuration.global_params.includePaths) in
  let macros = List.map (fun x -> " -D"^x) (!Configuration.global_params.macros) in
  let res = "\""^(String.concat " " includePaths)^(String.concat " " macros)^"\"" in
  res

let source_files () =
  let sourceFiles = List.map (fun x -> (!rootPath)^"/"^x) (!Configuration.global_params.sourceFiles) in
  (String.concat " " sourceFiles)
  
let kernel_args () = 
  let args = ref "" in
  let add_arg arg = args := !args^arg in
  let not_empty s = not (String.equal s "") in
  if not_empty (!Configuration.global_params.machdep) then add_arg (" -machdep=\""^(!Configuration.global_params.machdep)^"\"");
  let generatedSpecCustom = String.concat "," (!Configuration.global_params.generatedSpecCustom) in
  if not_empty generatedSpecCustom then add_arg (" -generated-spec-custom=\""^generatedSpecCustom^"\"");
  if (not (!Configuration.global_params.keepUnusedSpecifiedFunctions)) then add_arg " -remove-unused-specified-functions";
  if (!Configuration.global_params.aggressiveMerging) then add_arg " -aggressive-merging";
  add_arg " -kernel-warn-key annot-error=active  -kernel-warn-key too-large-array=active";
  add_arg " -no-unicode";
  add_arg " -inline-calls @inline -remove-inlined @inline ";
  !args

let global_metrics_args () = 
  let args = ref "" in
  let add_arg arg = args := !args^arg in
  let not_empty s = not (String.equal s "") in
  add_arg " -metrics";
  add_arg " -metrics-by-function";
  if not_empty (!Configuration.global_params.metricsOutput)
    then add_arg (" -metrics-output=\""^(!rootPath)^"/"^(Filename.remove_extension !Configuration.global_params.metricsOutput)^".txt\"")
    else add_arg (" -metrics-output=\"project_metrics.txt\"");
  !args 

let callgraph_args file () = 
  let args = ref "" in
  let add_arg arg = args := !args^arg in
  let not_empty s = not (String.equal s "") in
  if not_empty (!Configuration.global_params.cgOutput) then add_arg (" -cg=\""^(!rootPath^"/"^(!Configuration.global_params.cgOutput))^".dot\"") else add_arg (" -cg=\""^file^".dot\"");
  (* 'key:value' args *)
  let cgRoots = String.concat "," (!Configuration.global_params.cgRoots) in
  if not_empty cgRoots then add_arg (" -cg-roots=\""^cgRoots^"\"");
  if (!Configuration.global_params.cgServices) then add_arg " -cg-services" else add_arg " -cg-no-services";
  !args

let get_cg_output_file file () = 
  let not_empty s = not (String.equal s "") in
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
    curr_char := (Bytes.to_string contlenbuf);
    res := !res ^ !curr_char;
  done;
  ignore (Unix.read sock contlenbuf 0 1); (* consume remaining "\r\n" from request header *) (* note : why 1 ? *)
  !res

let rec had_errors_in_channel ic =
  try
    let msg = Stdlib.input_line ic in
    Lsp.Self.debug "\t%s\n%!" (msg);
    if (Utils.contains msg ~suffix: "FRAMA-C EXIT CODE: 0") then false
    else had_errors_in_channel ic
  with End_of_file -> Lsp.Self.debug "\n%!"; true


let execute_command command didSave ?id () = 
  let had_errors = ref true in 
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
  had_errors := had_errors_in_channel ic;
  (* Lsp.Self.debug ~level:4 "before accept\n%!"; *)
  match !had_errors, didSave with 
  | true, false ->
    Lsp.Self.debug ~level:2 "Error while executing frama-c command\n%!";
    (* Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac)); *)
    let lsp_error_message = Lsp_types.ResponseError.create ~code:(-32603) ~message:!msg () in
    let lsp_message = (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:response_id ~error: lsp_error_message ()) in
    let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in
    Unix.close wrapper_sock;
    data
  | false, false ->
      Lsp.Self.debug ~level:2 "No Error while executing frama-c command\n%!";
      let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "No problem found !") () in
      let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
      let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
      Unix.close wrapper_sock;
      data  
  | _, true ->
    Lsp.Self.debug ~level:2 "Error while executing frama-c command 0 \n%!";
    let (plugin_sock, _) = Unix.accept wrapper_sock in
    let data_size = getnumber (readcontlen plugin_sock) in
    let buffer = Bytes.make data_size '0' in
    let _req_data_len = Unix.read plugin_sock buffer 0 data_size in
    let request_str = (Bytes.to_string buffer) in
    (* Lsp.Self.debug ~level:4 "accept\n%!"; *)
    ignore (Unix.close_process_in ic);
    (* let _bytes_read = Unix.recv plugin_sock buffer 0 (Bytes.length buffer) [] in *) 
    (* Lsp.Self.debug ~level:4 "recv\n%!"; *)
    Unix.close plugin_sock;
    Unix.close wrapper_sock;
    request_str

let capabilities_str = {|{
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
      Lsp_types.CONTENT (capabilities_str);
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
      let kernel_opt = KernelOpt.create () in
      let lsp_opt = LspOpt.create (FindDefinition_feature ((Utils.id_to_int request.id), src_file, line, ch)) in
      let command = Command.create ~kernel:kernel_opt ~files:[files_to_parse] ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      (* let command = Printf.sprintf "frama-c %s %s %s -then -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-id=\"%s\" -lsp-root-path=\"%s\" -lsp-definition=%s:%s:%s ; echo \"FRAMA-C EXIT CODE: $?\" "
      files_to_parse (cpp_extra_args ()) (kernel_args ())
      (debug ()) (Stdlib.string_of_int (Utils.id_to_int request.id)) (!rootPath)
      src_file (Stdlib.string_of_int line) (Stdlib.string_of_int ch) in *)
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data = execute_command command_str false ~id:request.id () in
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
      let command = "frama-c "^files_to_parse^" "^(cpp_extra_args ())^(kernel_args ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\" -lsp-root-path=\""^(!rootPath)^"\" -lsp-declaration="^_file^":"^(Stdlib.string_of_int line)^":"^(Stdlib.string_of_int ch) ^ " ; echo \"FRAMA-C EXIT CODE: $?\"" in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
      let data = execute_command command false ~id:request.id () in
      Lsp_types.CONTENT (data);
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
      let _files = 
        String.concat " " 
        (match (String.ends_with ~suffix:".h" file) with
        | true -> file::(Utils.get_corr_cfile (!rootPath) file); 
        | false -> [file])
      in
      let files_to_parse =
        match (String.ends_with ~suffix:".c" file), source_files () with
          | true, _ -> file
          | _, "" -> file
          | _, _ -> (source_files ())
      in
      let command = "frama-c "^files_to_parse^(cpp_extra_args ())^(kernel_args ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\" -lsp-root-path=\""^(!rootPath)^"\" -lsp-show-povc=\""^file^":"^line^":"^ch^"\""^(wp_args ()) ^ " ; echo \"FRAMA-C EXIT CODE: $?\"" in
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
      let command = "frama-c -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-id=\""^(Stdlib.string_of_int (Utils.id_to_int request.id))^"\" -lsp-completion=\""^file^":"^line^":"^ch^"\"" ^ " ; echo \"FRAMA-C EXIT CODE: $?\"" in
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
    let lsp_message = registerCapabilityRequest (registrationParams ([registration "workspace/didChangeConfiguration"])) in
    Lsp_types.CONTENT (Json.save_string (lsp_message))

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
    let command = "frama-c -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-did-close=" ^ _file ^ " ; echo \"FRAMA-C EXIT CODE: $?\"" in
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
        let kernel_opt = KernelOpt.create () in
        let uncast_opt = UncastOpt.create () in
        let wp_opt = WpOpt.create () in
        let metacsl_opt = MetacslOpt.create () in
        let lsp_opt = LspOpt.create (DidSave_feature) in
        let command = Command.create ~kernel:kernel_opt ~files:[file_name] ~uncast:uncast_opt ~wp:wp_opt ~metacsl:metacsl_opt ~lsp:lsp_opt () in
        let command_str = (Command.string_of_t command) in
        (* let command = "frama-c -lsp -lsp-no-cmdline -lsp-debug="^(debug ()) ^ " " ^ file_name ^ " " ^ (cpp_extra_args ())^(kernel_args ())^" -uncast -then-last -wp -wp-rte -wp-prop @assigns -wp-gen -wp-no-simpl -wp-no-let -wp-no-filter -wp-no-core -wp-no-pruning -wp-no-clean -wp-no-ground -wp-no-extensional -wp-no-reduce -wp-no-parasite -wp-no-init-summarize-array -wp-no-simplify-is-cint -wp-no-simplify-land-mask -wp-no-prenex -wp-no-simplify-forall -wp-no-simplify-type -wp-bound-forall-unfolding 1 -lsp-did-save ; echo \"FRAMA-C EXIT CODE: $?\"" in *)
        Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
        Lsp_types.CONTENT (execute_command command_str true ());
      end
    else Lsp_types.EMPTY ()

  | "showGlobalMetrics" -> 
    Lsp.Self.debug ~level:4 "global metrics\n%!";
    let project_filename = if not (String.equal (!Configuration.global_params.metricsOutput) "") then (Filename.remove_extension !Configuration.global_params.metricsOutput) else "project_metrics" in
    let command = Printf.sprintf "frama-c %s %s %s -then %s -then -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-metrics=\"%s/%s\" ; echo \"FRAMA-C EXIT CODE: $?\""
    (source_files ()) (cpp_extra_args ()) (kernel_args ()) (global_metrics_args ()) (debug ()) (!rootPath) project_filename in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
    Lsp_types.CONTENT (execute_command command false ());

  | "displayCIL" -> 
      Lsp.Self.debug ~level:4 "displayCIL\n%!";
      let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for displayCIL \n%!"; assert false
      in
      let command = Printf.sprintf "frama-c %s %s %s -then -print -no-unicode -ocode \"%s_fc.c\" -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-display-cil=\"%s\" ; echo \"FRAMA-C EXIT CODE: $?\""
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
        let command = Printf.sprintf "frama-c %s %s %s -then -print -no-unicode -ocode \"%s_fc.c\" -no-annot -keep-comments -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-display-cil=\"%s\" ; echo \"FRAMA-C EXIT CODE: $?\""
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
    let command = Printf.sprintf "frama-c %s %s %s -then -metrics -metrics-by-function -metrics-output=\"%s.txt\" -then -lsp -lsp-no-cmdline -lsp-debug=%s -lsp-metrics=\"%s\" ; echo \"FRAMA-C EXIT CODE: $?\""
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
    let command = "frama-c "^file^(cpp_extra_args ())^(kernel_args ())^" -then"^(callgraph_args (Filename.remove_extension file) ())^" -then -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-compute-cg=\""^(get_cg_output_file (Filename.remove_extension file) ())^"\" ; echo \"FRAMA-C EXIT CODE: $?\"" in
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
  Lsp_types.CONTENT (Json.save_string (Lsp_types.ResponseError.json_of_t (error)))

let handle (json_string : string) server_sock : Lsp_types.lsp_result = 
  (* if !receivedShutdown then 
    Lsp_types.CONTENT (Shutdown.shutdown_error (Lsp_types.RequestMessage.t_of_json (Json.load_string json_string))) else  *)
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      if (List.exists (fun (key, _) -> key = "result") fields) then
        begin
          Lsp.Self.debug ~level:4 "result_handler\n%!";
          result_handler json_string 
        end
      else if (List.exists (fun (key, _) -> key = "error") fields) then 
        begin
          Lsp.Self.debug ~level:4 "error_handler\n%!";
          error_handler json_string
        end
      else if (not (List.exists (fun (key, _) -> key = "id") fields)) then 
        begin
          Lsp.Self.debug ~level:4 "notif_handler\n%!";
          notif_handler json_string server_sock
        end
      else if (List.exists (fun (key, _) -> key = "id") fields) then 
        begin
          Lsp.Self.debug ~level:4 "rq_handler\n%!";
          rq_handler json_string
        end
      else
        raise (Failure "Unknown request")
    | _ -> Lsp.Self.debug ~level:3 "no result\n%!"; raise (Failure "Unknown request")
  with
  | Json.Error _ -> raise (Failure "Unknown request")
    
