
let rootPath = ref ""
let receivedShutdown = ref false

let pid_list = ref []

type lsp_feature =
  | DidSave_feature
  (*| DidClose_feature of (string) *)
  | FindDefinition_feature of (int * string * int * int)
  | FindDeclaration_feature of (int * string * int * int)
  | ComputeCIL_feature
  | ComputeCallGraph_feature
  | ComputeMetrics_feature
  | ComputeProofObligation_feature of (string * int * string * int * int)
  | Prove_feature of (string * int * string * string * string)
  | ProveStrategies_feature of (string * int * string * string * string)

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
    keep_unused_specified_functions = !Configuration.global_params.removeUnusedSpecifiedFunctions;
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
    wp_prover: string list;
    wp_timeout: int;
    wp_session: string;
    wp_smoke_tests: bool;
    wp_smoke_timeout: int;
    wp_script: string
  }
  let create ?wp_fct ?wp_prop ?wp_timeout ?wp_prover ?wp_smoke_tests ?wp_gen ?wp_script () = {
    wp = true;
    wp_rte = !Configuration.global_params.wpRte;
    wp_prop = (match wp_prop with | None -> [] | Some p -> p);
    wp_fct = (match wp_fct with | None -> [] | Some f -> f);
    wp_gen = (match wp_gen with None -> true | Some s -> s);
    wp_pruning = !Configuration.global_params.wpPruning;
    wp_check_memory_model = !Configuration.global_params.wpCheckMemoryModel;
    wp_no_volatile = !Configuration.global_params.wpVolatile;
    wp_prover = (match wp_prover with | None -> [!Configuration.global_params.wpProver]; | Some p -> p);
    wp_timeout = (match wp_timeout with None -> !Configuration.global_params.wpTimeout | Some t -> t);
    wp_session = !Configuration.global_params.wpSession;
    wp_smoke_tests = (match wp_smoke_tests with None -> false | Some s -> s);
    wp_smoke_timeout = 3;
    wp_script = (match wp_script with | None -> !Configuration.global_params.wpScript | Some s -> s);
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let wp_opt = option_if_true options.wp "-wp" in
    let wp_prop_opt = option_if_not_empty_string (String.concat "," options.wp_prop) "-wp-prop " in
    let wp_fct_opt = option_if_not_empty_string (String.concat "," options.wp_fct) "-wp-fct " in
    let wp_gen_opt = option_if_true options.wp_gen "-wp-gen" in
    let wp_rte_opt = option_if_true options.wp_rte "-wp-rte" in
    let wp_pruning_opt = option_if_true options.wp_pruning "-wp-no-pruning" in
    let wp_check_memory_model_opt = option_if_true options.wp_check_memory_model "-wp-model \"Typed+var+int+float\" -wp-check-memory-model" in
    let wp_no_volatile_opt = option_if_true options.wp_no_volatile "-wp-no-volatile" in
    let wp_prover_opt = option_if_not_empty_string (String.concat "," options.wp_prover) "-wp-prover " in
    let wp_timeout_opt = Printf.sprintf "-wp-timeout %d" options.wp_timeout in
    let wp_session_opt = option_if_not_empty_string options.wp_session "-wp-session " in
    let wp_smoke_tests_opt = option_if_true options.wp_smoke_tests "-wp-smoke-tests " in
    let wp_smoke_timeout_opt = option_if_not_empty_string (Stdlib.string_of_int options.wp_smoke_timeout) "-wp-smoke-timeout " in
    let wp_script = option_if_not_empty_string (options.wp_script) "-wp-script " in
    Printf.sprintf "%s %s %s %s %s %s %s %s %s %s %s %s %s %s" wp_opt wp_prop_opt wp_fct_opt wp_gen_opt wp_rte_opt wp_pruning_opt wp_check_memory_model_opt wp_no_volatile_opt wp_prover_opt wp_timeout_opt wp_session_opt wp_smoke_tests_opt wp_smoke_timeout_opt wp_script
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
    meta = !Configuration.global_params.metacslActive;
    meta_warn_key = "unknown-func=active";
    meta_checks = !Configuration.global_params.metacslChecks;
    meta_no_simpl = !Configuration.global_params.metacslNoSimpl;
    meta_no_check_ext = !Configuration.global_params.metacslNoCheckExt;
    meta_number_assertions = !Configuration.global_params.metacslNumberAssertions;
    meta_check_callee_assigns = !Configuration.global_params.metacslCheckCalleeAssigns
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
    if options.meta then 
    Printf.sprintf "%s %s %s %s %s %s %s %s" meta_opt meta_warn_key_opt meta_warn_key_opt meta_checks_opt meta_no_simpl_opt meta_no_check_ext_opt meta_number_assertions_opt meta_check_callee_assigns_opt
    else ""
end

module UncastOpt = struct
  type t = {
    uncast: bool;
    uncast_endianness: string;
    uncast_lshift_as_mul: bool;
    uncast_rshift_as_div: bool
  }
  let create () = {
    uncast = !Configuration.global_params.uncastActive;
    uncast_endianness = !Configuration.global_params.uncastEndianness;
    uncast_lshift_as_mul = !Configuration.global_params.uncastLshiftAsMul;
    uncast_rshift_as_div = !Configuration.global_params.uncastRshiftAsDiv
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let uncast_opt = option_if_true options.uncast "-uncast " in
    let uncast_endianness_opt = option_if_not_empty_string options.uncast_endianness "-uncast-endianness " in
    let uncast_lshift_as_mul_opt = option_if_true options.uncast_lshift_as_mul "-uncast-lshift-as-mul " in
    let uncast_rshift_as_div_opt = option_if_true options.uncast_rshift_as_div "-uncast-rshift-as-div " in
    if options.uncast then Printf.sprintf "%s %s %s %s" uncast_opt uncast_endianness_opt uncast_lshift_as_mul_opt uncast_rshift_as_div_opt
    else ""
end

module MetricsOpt = struct
  type t = {
    metrics : bool;
    metrics_by_function: bool;
    metrics_output: string;
  }
  let create ?file () = {
    metrics = true;
    metrics_by_function = true;
    metrics_output = (match file with
    | None -> (if not (String.trim !Configuration.global_params.metricsOutput = "") then !Configuration.global_params.metricsOutput else ".frama-c/fc_metrics.txt")
    | Some f -> Printf.sprintf ".frama-c/fc_%s.txt" f)
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let metrics_opt = option_if_true options.metrics "-metrics " in
    let metrics_by_function_opt = option_if_true options.metrics_by_function "-metrics-by-function" in
    let metrics_output_opt = option_if_not_empty_string options.metrics_output "-metrics-output " in
    Printf.sprintf "%s %s %s" metrics_opt metrics_by_function_opt metrics_output_opt
end


module PprintOpt = struct
  type t = {
    print : bool;
    no_unicode : bool;
    ocode : string;
    no_annot : bool;
    keep_comments : bool
  }
  let create ~file ?no_annot () = {
    print = true;
    no_unicode = true;
    ocode = Printf.sprintf ".frama-c/fc_%s.c" file;
    no_annot = (match no_annot with None -> false | Some v -> v);
    keep_comments = false;
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let print_opt = option_if_true options.print "-print " in
    let no_unicode_opt = option_if_true options.no_unicode "-no-unicode " in
    let ocode_opt = option_if_not_empty_string options.ocode "-ocode " in
    let no_annot_opt = option_if_true options.no_annot "-no-annot " in
    let keep_comments_opt = option_if_true options.keep_comments "-keep-comments" in
    Printf.sprintf "%s %s %s %s %s" print_opt no_unicode_opt ocode_opt no_annot_opt keep_comments_opt
end

module CgOpt = struct
  type t = {
    cg : string;
    cg_roots : string list;
    cg_services : bool;
    cg_no_services : bool;
    cmd : string;
  }
  let create ?file () : t = 
    let ofile = (match file with None -> !Configuration.global_params.cgOutput | Some f -> Printf.sprintf ".frama-c/fc_%s.dot" f) in
    {
    cg = ofile;
    cg_roots = !Configuration.global_params.cgRoots;
    cg_services = !Configuration.global_params.cgServices;
    cg_no_services = not !Configuration.global_params.cgServices;
    cmd = Printf.sprintf "dot -Tpdf %s -o %s.pdf" ofile ofile;
    }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let cg_opt = option_if_not_empty_string options.cg "-cg " in
    let cg_roots_opt = option_if_not_empty_string (String.concat "," options.cg_roots) "-cg-roots=" in
    let cg_services_opt = option_if_true options.cg_services "-cg-services" in
    let cg_no_services_opt = option_if_true options.cg_no_services "-cg-no-services" in
    Printf.sprintf "%s %s %s %s" cg_opt cg_roots_opt cg_services_opt cg_no_services_opt
end


module LspOpt = struct
  type t = lsp_feature
  let create (feature : t) = feature
  let string_of_t (feature : t) : string =
    match feature with
    | DidSave_feature -> "-lsp-did-save"
    (* | DidClose_feature (file) -> Printf.sprintf "-lsp-did-close=%s" file *)
    | FindDefinition_feature (id, file, line, column) -> Printf.sprintf "-lsp-id=\"%d\" -lsp-definition=%s:%d:%d" id file line column
    | FindDeclaration_feature (id, file, line, column) -> Printf.sprintf "-lsp-id=\"%d\" -lsp-declaration=%s:%d:%d" id file line column
    | ComputeCIL_feature -> ""
    | ComputeCallGraph_feature -> ""
    | ComputeMetrics_feature -> ""
    | ComputeProofObligation_feature (root_path, id, file, line, column) -> Printf.sprintf "-lsp-root-path=\"%s\" -lsp-id=\"%d\" -lsp-show-povc=%s:%d:%d" root_path id file line column
    | Prove_feature (root_path, id, file, fct, prop) -> Printf.sprintf "-lsp-root-path=\"%s\" -lsp-id=\"%d\" -lsp-prove=%s:%s:%s" root_path id file fct prop
    | ProveStrategies_feature (root_path, id, file, fct, prop) -> Printf.sprintf "-lsp-root-path=\"%s\" -lsp-id=\"%d\" -lsp-prove=%s:%s:%s" root_path id file fct prop
end


module Command = struct
  type t = {
  verbose: int;
  files : string list option;
  kernel : KernelOpt.t option;
  wp : WpOpt.t option;
  metacsl : MetacslOpt.t option;
  uncast : UncastOpt.t option;
  metrics : MetricsOpt.t option;
  pprint : PprintOpt.t option;
  cg : CgOpt.t option;
  lsp : LspOpt.t option;
  }
  let create ?kernel ?files ?wp ?metacsl ?uncast ?metrics ?pprint ?cg ?lsp () : t = {
    verbose = !Configuration.global_params.acslLsp;
    files = (match kernel, files with
      | None, _ -> None
      | _, Some _ -> files;
      | _, None -> 
        let sourceFiles = List.map (fun x -> (!rootPath)^"/"^x) (!Configuration.global_params.sourceFiles) in
        match sourceFiles with
        | [] -> Some (Utils.get_workspace_files !rootPath)
        | _ -> Some (sourceFiles)
    );
    kernel = kernel;
    wp = wp;
    metacsl = metacsl;
    uncast = uncast;
    metrics = metrics;
    pprint = pprint;
    cg = cg;
    lsp = lsp
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (s ^ " " ^ opt) else "" in
    let file_names = match options.files with None -> "" | Some f -> String.concat " " f in
    let debug_level = Stdlib.string_of_int options.verbose in
    let common_opt = Printf.sprintf "frama-c -lsp -lsp-no-cmdline -lsp-debug=%s %s" debug_level file_names in
    let kernel_opt = match options.kernel with None -> "" | Some k -> KernelOpt.string_of_t k in
    let uncast_opt = match options.uncast with None -> "" | Some u -> option_if_not_empty_string (UncastOpt.string_of_t u) "-then-last" in
    let wp_opt = match options.wp with None -> "" | Some w -> option_if_not_empty_string (WpOpt.string_of_t w) "" in
    let metrics_opt = match options.metrics with None -> "" | Some m -> option_if_not_empty_string (MetricsOpt.string_of_t m) "" in
    let pprint_opt = match options.pprint with None -> "" | Some p -> option_if_not_empty_string (PprintOpt.string_of_t p) "" in
    let metacsl_opt = match options.metacsl with None -> "" | Some m -> option_if_not_empty_string (MetacslOpt.string_of_t m) "-then-last" in
    let cg_opt = match options.cg with None -> "" | Some c -> option_if_not_empty_string (CgOpt.string_of_t c) "" in
    let lsp_opt = match options.lsp with None -> "" | Some l -> option_if_not_empty_string (LspOpt.string_of_t l) "" in
    let frama_c_cmd = Printf.sprintf "%s %s %s %s %s %s %s %s %s" common_opt kernel_opt uncast_opt metacsl_opt wp_opt metrics_opt pprint_opt cg_opt lsp_opt in
    let exit_value_cmd = "echo \"FRAMA-C EXIT CODE: $?\"" in
    let session_dir_cmd = "mkdir -p .frama-c" in
    let cg_cmd = match options.cg with None -> "" | Some c -> Printf.sprintf "%s" c.cmd in
    Printf.sprintf "%s; %s; %s; %s" session_dir_cmd frama_c_cmd exit_value_cmd cg_cmd
  end



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

let get_notification_list filename dlist accumulated_list =
  let lsp_notification_params = Lsp_types.PublishDiagnosticsParams.create ~uri:filename ~diagnostics:dlist () in
  let json_notification_params = Lsp_types.PublishDiagnosticsParams.json_of_t lsp_notification_params in
  let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:json_notification_params () in
  let json_notification = Lsp_types.NotificationMessage.json_of_t lsp_notification in
  let data = Json.save_string (json_notification) in
  data :: accumulated_list

let update_diag_data msg severity =
  try
    let msg_list = Str.split (Str.regexp ":") msg in
    let filename, pos =
      match msg_list with
        | filename :: pos :: _ -> filename, (Stdlib.int_of_string pos)
        | _ -> "", 0
    in
    Lsp.Self.debug "\t%s ---- %d\n%!" (filename) pos;
    let pos = Utils.to_filepath_position filename pos 0 in
    let loc = Utils.real_loc (pos, pos) in
    let source = "kernel" in
    let diag = Lsp_types.Diagnostic.create ~range:(Utils.get_lsp_range loc) ~severity:severity ~message:msg ~source:source () in
    let diag_list = DidSave.StringMap.find_opt filename !DidSave.diag_map in
    let diag_list = match diag_list with
      | None -> []
      | Some l -> l
    in
    let dlist = (diag :: diag_list) in
    DidSave.diag_map := DidSave.StringMap.add filename dlist !DidSave.diag_map;
  with _ -> ()

let rec had_errors_in_channel oc =
  try
    let msg = Stdlib.input_line oc in
    Lsp.Self.debug "\t%s\n%!" (msg);
    if (Utils.contains msg ~suffix: "FRAMA-C EXIT CODE: 0") then false
    else if (Utils.contains msg ~suffix: ": fatal error:") then (
      update_diag_data msg Lsp_types.DiagnosticSeverity.Error;
      had_errors_in_channel oc)
    else if (Utils.contains msg ~suffix: ": warning:") then (
      update_diag_data msg Lsp_types.DiagnosticSeverity.Warning;
      had_errors_in_channel oc)
    else if (Utils.contains msg ~suffix: ": note:") then (
      update_diag_data msg Lsp_types.DiagnosticSeverity.Warning;
      had_errors_in_channel oc)
    else had_errors_in_channel oc
  with End_of_file -> Lsp.Self.debug "\n%!"; true




let execute_command command feature =
  let wrapper_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind wrapper_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, 8006));
  Unix.listen wrapper_sock 100;
  let env = Unix.environment () in
  let ic, oc, ec = Unix.open_process_full command env in
  let frama_c_exit_with_error = had_errors_in_channel ic in
  let _ = had_errors_in_channel ec in
  let special_errors = DidSave.StringMap.fold get_notification_list !DidSave.diag_map [] in
  Lsp.Self.debug ~level:2 "Read ic completed !!!\n%!";
  match frama_c_exit_with_error with
  | true ->
    (
    match feature with
    | DidSave_feature ->
      Lsp.Self.debug ~level:2 "Executed frama-c command\n%!";
      let (plugin_sock, _) = Unix.accept wrapper_sock in
      let data_size = getnumber (readcontlen plugin_sock) in
      let buffer = Bytes.make data_size '0' in
      let _req_data_len = Unix.read plugin_sock buffer 0 data_size in
      let request_str = (Bytes.to_string buffer) in
      ignore (Unix.close_process_full (ic, oc, ec));
      Unix.close plugin_sock;
      Unix.close wrapper_sock;
      let data =
        match special_errors, (String.trim request_str = "") with
        | [], true -> ""
        | [], false -> request_str
        | l, true -> String.concat ":::" l
        | l, false -> (String.concat ":::" l) ^ ":::" ^ request_str
      in
      data

    | FindDefinition_feature (id, _file, _, _)
    | FindDeclaration_feature (id, _file, _, _)
    | ComputeProofObligation_feature (_, id, _file, _, _)
    | Prove_feature (_, id, _file, _, _)
    | ProveStrategies_feature (_, id, _file, _, _) ->
      Lsp.Self.debug ~level:2 "Error while executing frama-c command\n%!";
      let msg = Printf.sprintf "Frama-C Error ! Check OUTPUT !" in
      let lsp_error_message = Lsp_types.ResponseError.create ~code:(-32603) ~message:msg () in
      let lsp_message = (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~error:lsp_error_message ()) in
      let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in
      Unix.close wrapper_sock;
      data

    | ComputeCIL_feature
    | ComputeCallGraph_feature
    | ComputeMetrics_feature ->
      let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Error ~message: "Frama-c error ! Check OUTPUT !" () in
      let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
      let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
      Unix.close wrapper_sock;
      data
    )
  | false ->
    (
    match feature with
      | DidSave_feature
      | FindDefinition_feature (_, _, _, _)
      | FindDeclaration_feature (_, _, _, _)
      | ComputeProofObligation_feature (_, _, _, _, _)
      | Prove_feature (_, _, _, _, _)
      | ProveStrategies_feature (_, _, _, _, _) ->
        Lsp.Self.debug ~level:2 "Executed frama-c command\n%!";
        let (plugin_sock, _) = Unix.accept wrapper_sock in
        let data_size = getnumber (readcontlen plugin_sock) in
        let buffer = Bytes.make data_size '0' in
        let _req_data_len = Unix.read plugin_sock buffer 0 data_size in
        let request_str = (Bytes.to_string buffer) in
        ignore (Unix.close_process_full (ic, oc, ec));
        Unix.close plugin_sock;
        Unix.close wrapper_sock;
        let data =
          match special_errors, (String.trim request_str = "") with
          | [], true -> ""
          | [], false -> request_str
          | l, true -> String.concat ":::" l
          | l, false -> (String.concat ":::" l) ^ ":::" ^ request_str
        in
        data  
      | ComputeCIL_feature
      | ComputeCallGraph_feature
      | ComputeMetrics_feature ->
        Lsp.Self.debug ~level:2 "No Error after executing frama-c command\n%!";
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Successful operation !") () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        Unix.close wrapper_sock;
        data
    )

let fork_execute_command command feature =
  let pid = Unix.fork () in
  if not (pid = 0) then (
    pid_list := pid :: !pid_list;
    Lsp.Self.debug ~level:2 "Request sent to Frama-C !\n%!";
    let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Request sent to Frama-C !") () in
    let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
    let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
    data, pid
  )
  else
    try
      (execute_command command feature), pid
    with _exn -> (match feature with
      | FindDefinition_feature (id, _file, _, _)
      | FindDeclaration_feature (id, _file, _, _)
      | ComputeProofObligation_feature (_, id, _file, _, _)
      | Prove_feature (_, id, _file, _, _)
      | ProveStrategies_feature (_, id, _file, _, _) ->
        let lsp_error_message = Lsp_types.ResponseError.create ~code:(-32603) ~message:"Server is busy !" () in
        let lsp_message = (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~error:lsp_error_message ()) in
        let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in
        data, pid
      | _ ->
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Error ~message: "Server is busy !" () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        data, pid)


    (* "completionProvider": {
              "triggerCharacters": [],
              "allCommitCharacters": [],
              "resolveProvider": false,
              "completionItem": {
                "labelDetailsSupport": false
              }
            }, *)

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
      Lsp_types.CONTENT (capabilities_str), 1;
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
      let kernel_opt = KernelOpt.create () in
      let feature = FindDefinition_feature ((Utils.id_to_int request.id), src_file, line, ch) in
      let lsp_opt = LspOpt.create (feature) in
      let command = Command.create ~kernel:kernel_opt ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data, pid = fork_execute_command command_str feature in
      Lsp_types.CONTENT data, pid;
      
    | "textDocument/declaration" -> 
      Lsp.Self.debug ~level:4 "declaration\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.DeclarationParams.t_of_json p
        | None -> Lsp.Self.debug ~level:3 "No declaration params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let src_file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
      let line = params.position.line in 
      let ch = params.position.character in
      let kernel_opt = KernelOpt.create () in
      let feature = FindDeclaration_feature ((Utils.id_to_int request.id), src_file, line, ch) in
      let lsp_opt = LspOpt.create (feature) in
      let command = Command.create ~kernel:kernel_opt ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data, pid = fork_execute_command command_str feature in
      Lsp_types.CONTENT (data), pid;

    | "showPOVC" -> (* show proof obligation of specific function *)
      let id = (Utils.id_to_int request.id) in
      Lsp.Self.debug ~level:4 "showPOVC, %d\n%!" id;
      let (file, line, ch) = match request.params with 
          | Some `List 
            [`List 
              [`String f; `Assoc [
                "line", `Int l;
                "character", `Int c;
              ]]] -> 
            (Utils.remove_newline (Utils.remove_quotes (f)), l, c)
          | _ -> Lsp.Self.debug ~level:3 "No params for showPOVC \n%!"; assert false
        in
      let kernel_opt = KernelOpt.create () in
      let uncast_opt = UncastOpt.create () in
      let wp_opt = WpOpt.create () in
      let feature = ComputeProofObligation_feature (!rootPath, id, file, line, ch) in
      let lsp_opt = LspOpt.create (feature) in
      let command = 
        match (String.ends_with ~suffix:".c" file) with
        | true -> Command.create ~kernel:kernel_opt ~files:[file] ~uncast:uncast_opt ~wp:wp_opt ~lsp:lsp_opt ()
        | false -> Command.create ~kernel:kernel_opt ~uncast:uncast_opt ~wp:wp_opt ~lsp:lsp_opt ()
      in
      let command_str = (Command.string_of_t command) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data, pid = fork_execute_command command_str feature in
      Lsp_types.CONTENT (data), pid;

    | "provePO" -> (* prove with WP *)
      let id = (Utils.id_to_int request.id) in
      Lsp.Self.debug ~level:4 "provePO, %d\n%!" id;
      let (file, fct, prop) = match request.params with
          | Some `List
            [`List 
              [`String f;
              `String function_name;
              `String property_name
              ]] -> 
            (Utils.remove_newline (Utils.remove_quotes (f)), function_name, property_name)
          | _ -> Lsp.Self.debug ~level:3 "No params for showPOVC \n%!"; assert false
        in
      let kernel_opt = KernelOpt.create () in
      let uncast_opt = UncastOpt.create () in
      let wp_opt = WpOpt.create ~wp_fct:[fct] ~wp_prop:[prop] ~wp_prover:["alt-ergo"] ~wp_gen:false ~wp_timeout:10 () in
      let feature = Prove_feature (!rootPath, id, file, fct, prop) in
      let lsp_opt = LspOpt.create (feature) in
      let command = 
        match (String.ends_with ~suffix:".c" file) with
        | true -> Command.create ~kernel:kernel_opt ~files:[file] ~uncast:uncast_opt ~wp:wp_opt ~lsp:lsp_opt ()
        | false -> Command.create ~kernel:kernel_opt ~uncast:uncast_opt ~wp:wp_opt ~lsp:lsp_opt ()
      in
      let command_str = (Command.string_of_t command) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data, pid = fork_execute_command command_str feature in
      Lsp_types.CONTENT (data), pid;

      | "provePOStrategies" -> (* prove with WP strategies *)
      let id = (Utils.id_to_int request.id) in
      Lsp.Self.debug ~level:4 "provePOStrategies, %d\n%!" id;
      let (file, fct, prop) = match request.params with
          | Some `List
            [`List 
              [`String f;
              `String function_name;
              `String property_name
              ]] -> 
            (Utils.remove_newline (Utils.remove_quotes (f)), function_name, property_name)
          | _ -> Lsp.Self.debug ~level:3 "No params for showPOStrategies \n%!"; assert false
        in
      let kernel_opt = KernelOpt.create () in
      let uncast_opt = UncastOpt.create () in
      let wp_opt = WpOpt.create ~wp_fct:[fct] ~wp_prop:[prop] ~wp_prover:["tip"] ~wp_gen:false ~wp_timeout:10 ~wp_script:"dry" () in
      let feature = ProveStrategies_feature (!rootPath, id, file, fct, prop) in
      let lsp_opt = LspOpt.create (feature) in
      let command = 
        match (String.ends_with ~suffix:".c" file) with
        | true -> Command.create ~kernel:kernel_opt ~files:[file] ~uncast:uncast_opt ~wp:wp_opt ~lsp:lsp_opt ()
        | false -> Command.create ~kernel:kernel_opt ~uncast:uncast_opt ~wp:wp_opt ~lsp:lsp_opt ()
      in
      let command_str = (Command.string_of_t command) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data, pid = fork_execute_command command_str feature in
      Lsp_types.CONTENT (data), pid;
(*
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
      Lsp_types.CONTENT (fork_execute_command command false ~id:request.id ());
*)
    | "shutdown" -> receivedShutdown := true;
      let lsp_response = Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:request.id ~result:`Null ()) in
      Lsp_types.CONTENT (Json.save_string lsp_response), 1;
    | _ -> 
      Lsp_types.CONTENT (Json.save_string `Null), 1
  with exn ->  
    Lsp.Self.debug ~level:3 "Backtrace : %s\n" (Printexc.get_backtrace ());
    Lsp_types.CONTENT (Json.save_string (Utils.make_error (Printexc.to_string (exn)) (Utils.id_to_int id))), 1


let notif_handler json_string server_sock =
  let json = Json.load_string json_string in 
  let notif = Lsp_types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    Lsp.Self.debug ~level:4 "initialized\n%!";
    send_request server_sock (Json.save_string Configuration.request_configurations);
    let registrations = [Lsp_types.Registration.create ~id:"registration" ~method_:"workspace/didChangeConfiguration" ()] in
    let lsp_registration_param = Lsp_types.RegistrationParams.create ~registrations:registrations () in
    let json_registration_params = Lsp_types.RegistrationParams.json_of_t lsp_registration_param in
    let lsp_request = Lsp_types.RequestMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Str "register_capability") ~method_:"client/registerCapability" ~params:json_registration_params () in
    let json_request = Lsp_types.RequestMessage.json_of_t lsp_request in
    let data = Json.save_string (json_request) in
    Lsp_types.CONTENT (data), 1

    (*
  | "textDocument/didOpen" ->
    Lsp.Self.debug ~level:4 "didOpen\n%!";
    let params = match notif.params with 
      | Some p -> Lsp_types.DidOpenTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in 
    let _file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    let command = "frama-c"^(cpp_extra_args ())^(kernel_args ())^" -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-did-open=" ^ _file in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command;
    Lsp_types.CONTENT ((fork_execute_command command false)); *)

    (*
  | "textDocument/didClose" ->
    Lsp.Self.debug ~level:4 "didClose\n%!";
    let params = match notif.params with 
      | Some p -> Lsp_types.DidCloseTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in 
    let src_file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    let lsp_opt = LspOpt.create (DidClose_feature(src_file)) in
    let command = Command.create ~lsp:lsp_opt () in
    let command_str = (Command.string_of_t command) in
    (* let command = "frama-c -lsp -lsp-no-cmdline -lsp-debug="^(debug ())^" -lsp-did-close=" ^ src_file ^ " ; echo \"FRAMA-C EXIT CODE: $?\"" in *)
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
    Lsp_types.CONTENT (fork_execute_command command_str false ());
    *)

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
        let wp_opt = WpOpt.create ~wp_prop:["@assigns"] ~wp_prover:["none"] ~wp_smoke_tests:false ~wp_gen:true () in
        let metacsl_opt = MetacslOpt.create () in
        let feature = DidSave_feature in
        let lsp_opt = LspOpt.create (feature) in
        let command = Command.create ~kernel:kernel_opt ~files:[file_name] ~uncast:uncast_opt ~wp:wp_opt ~metacsl:metacsl_opt ~lsp:lsp_opt () in
        let command_str = (Command.string_of_t command) in
        Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
        let data = execute_command command_str feature in
        Lsp_types.CONTENT (data), 1;
      end
    else Lsp_types.EMPTY (), 1
    (*
      begin
        let kernel_opt = KernelOpt.create () in
        let uncast_opt = UncastOpt.create () in
        let feature = DidSave_feature in
        let lsp_opt = LspOpt.create (feature) in
        let command = Command.create ~kernel:kernel_opt ~uncast:uncast_opt ~lsp:lsp_opt () in
        let command_str = (Command.string_of_t command) in
        Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
        let data = execute_command command_str feature in
        Lsp_types.CONTENT (data), 1
      end
      *)


  | "showGlobalMetrics" -> 
    Lsp.Self.debug ~level:4 "global metrics\n%!";
    let kernel_opt = KernelOpt.create () in
    let metrics_opt = MetricsOpt.create () in
    let feature = ComputeMetrics_feature in
    let command = Command.create ~kernel:kernel_opt ~metrics:metrics_opt () in
    let command_str = (Command.string_of_t command) in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
    let data, pid = fork_execute_command command_str feature in
    Lsp_types.CONTENT (data), pid;

  | "smokeTests" -> 
      Lsp.Self.debug ~level:4 "displayCIL\n%!";
      let file_name = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for displayCIL \n%!"; assert false
      in
      let kernel_opt = KernelOpt.create () in
      let uncast_opt = UncastOpt.create () in
      let wp_opt = WpOpt.create ~wp_prop:["smoke"] ~wp_prover:["alt-ergo"] ~wp_smoke_tests:true ~wp_gen:false () in
      let feature = DidSave_feature in
      let lsp_opt = LspOpt.create (feature) in
      let command = Command.create ~kernel:kernel_opt ~files:[file_name] ~uncast:uncast_opt ~wp:wp_opt ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data = execute_command command_str feature in
      Lsp_types.CONTENT (data), 1;

  | "displayCIL" -> 
      Lsp.Self.debug ~level:4 "displayCIL\n%!";
      let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for displayCIL \n%!"; assert false
      in
      let feature = ComputeCIL_feature in
      let kernel_opt = KernelOpt.create () in
      let file_basename = (Filename.remove_extension (Filename.basename (String.trim file))) in
      let pprint_opt = PprintOpt.create ~file:file_basename () in
      let command = Command.create ~kernel:kernel_opt ~files:[file] ~pprint:pprint_opt () in
      let command_str = (Command.string_of_t command) in
      Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
      let data, pid = fork_execute_command command_str feature in
      Lsp_types.CONTENT (data), pid;

  | "displayCIL_noannot" -> 
        Lsp.Self.debug ~level:4 "displayCIL_noannot\n%!";
        let file = match notif.params with 
          | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
          | _ -> Lsp.Self.debug ~level:3 "No params for displayCIL \n%!"; assert false
        in
        let feature = ComputeCIL_feature in
        let kernel_opt = KernelOpt.create () in
        let file_basename = (Filename.remove_extension (Filename.basename (String.trim file))) in
        let pprint_opt = PprintOpt.create ~file:file_basename ~no_annot:true () in
        let command = Command.create ~kernel:kernel_opt ~files:[file] ~pprint:pprint_opt () in
        let command_str = (Command.string_of_t command) in
        Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
        let data, pid = fork_execute_command command_str feature in
        Lsp_types.CONTENT (data), pid;

  | "showLocalMetrics" -> 
    Lsp.Self.debug ~level:4 "local metrics\n%!";
    let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for metrics \n%!"; assert false
    in
    let file_basename = (Filename.remove_extension (Filename.basename (String.trim file))) in
    let feature = ComputeMetrics_feature in
    let kernel_opt = KernelOpt.create () in
    let metrics_opt = MetricsOpt.create ~file:file_basename () in
    let command = Command.create ~kernel:kernel_opt ~files:[file] ~metrics:metrics_opt () in
    let command_str = (Command.string_of_t command) in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
    let data, pid = fork_execute_command command_str feature in
    Lsp_types.CONTENT (data), pid
  
  | "computeCG" -> 
    Lsp.Self.debug ~level:4 "computeCG\n%!";
    let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Lsp.Self.debug ~level:3 "No params for computeCG \n%!"; assert false
    in
    let feature = ComputeCallGraph_feature in
    let file_basename = (Filename.remove_extension (Filename.basename (String.trim file))) in
    let kernel_opt = KernelOpt.create () in
    let cg_opt = CgOpt.create ~file:file_basename () in
    let command = Command.create ~kernel:kernel_opt ~files:[file] ~cg:cg_opt () in
    let command_str = (Command.string_of_t command) in
    Lsp.Self.debug ~level:3 "Command = %s\n%!" command_str;
    let data, pid = fork_execute_command command_str feature in
    Lsp_types.CONTENT (data), pid

  | "workspace/didChangeConfiguration" ->
    Lsp.Self.debug ~level:4 "didChangeConfiguration\n%!";
    let data = Json.save_string (Configuration.request_configurations) in
    Lsp_types.CONTENT (data), 1;

  | "exit" -> if !receivedShutdown then Unix._exit 0 else Unix._exit 1
  | _ -> 
      Lsp_types.EMPTY (), 1



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
    Lsp_types.EMPTY (), 1;
  | _ -> 
    Lsp_types.EMPTY (), 1

(* todo : implement client error handling with different error codes *)
let error_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Lsp_types.ResponseMessage.t_of_json json in 
  let error = 
    match request.error with 
    | Some err -> err 
    | None -> Lsp.Self.debug ~level:3 "No error \n%!"; assert false
  in 
  let data = Json.save_string (Lsp_types.ResponseError.json_of_t (error)) in
  Lsp_types.CONTENT (data), 1

exception UnknownRequest of int

let handle (json_string : string) server_sock : (Lsp_types.lsp_result * int) =
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
        raise (UnknownRequest 1)
    | _ -> Lsp.Self.debug ~level:3 "no result\n%!"; raise (UnknownRequest 1)
  with
  | Json.Error _ -> raise (UnknownRequest 1)
    
