 (*************************************************************************
 *                                                                        *
 *  This file is part of Frama-C/LSP plugin.                              *
 *                                                                        *
 *  You can redistribute it and/or modify it under the terms of the GNU   *
 *  General Public License as published by the Free Software Foundation,  *
 *  version 3.                                                            *
 *                                                                        *
 *  It is distributed in the hope that it will be useful,                 *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *
 *  GNU General Public License for more details.                          *
 *                                                                        *
 *  See the GNU General Public License version 3                          *
 *  for more details (enclosed in the file LICENSE.md                     *
 *                                                                        *
 *************************************************************************)

let rootPath = ref ""
let receivedShutdown = ref false

let fork_pid = ref 0

type lsp_feature =
  | DidSave_feature
  | FindDefinition_feature of (int * string * int * int)
  | FindDeclaration_feature of (int * string * int * int)
  | ComputeCIL_feature
  | ComputeCallGraph_feature of string
  | ComputeMetrics_feature
  | ComputeProofObligation_feature of (string * int * string * int * int)
  | ComputeProofObligationID_feature of (int * string)
  | Prove_feature of (int * string * string * string)

module KernelOpt = struct
type t = {
  include_paths : string list;
  macros : string list;
  macroStrategiesFunctionPrefix : string;
  fct : string list;
  cpp_extra_args : string;
  machdep : string;
  generated_spec_custom : string list;
  keep_unused_specified_functions : bool;
  aggressive_merging : bool;
  kernel_warn_key : string;
  no_unicode : bool;
  inline_calls : string list;
  remove_inlined : string list;
  no_annot: bool;
  strategies : bool
  }
let create ?fct ~strategies () =
  {
    include_paths = !Configuration.global_params.includePaths;
    macros = !Configuration.global_params.macros;
    macroStrategiesFunctionPrefix = !Configuration.global_params.macroStrategiesFunctionPrefix;
    fct = (match fct with None -> [] | Some f -> f);
    cpp_extra_args = "-CC";
    machdep = !Configuration.global_params.machdep;
    generated_spec_custom = !Configuration.global_params.generatedSpecCustom;
    keep_unused_specified_functions = !Configuration.global_params.removeUnusedSpecifiedFunctions;
    aggressive_merging = !Configuration.global_params.aggressiveMerging;
    kernel_warn_key = "annot-error=active,too-large-array=active";
    no_unicode = true;
    inline_calls = !Configuration.global_params.inlineCalls;
    remove_inlined = !Configuration.global_params.removeInlined;
    no_annot = !Configuration.global_params.noAnnot;
    strategies = strategies;
  }
let string_of_t (options : t) : string =
  let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
  let option_if_true b opt = if b then (opt) else "" in
  let include_paths_opt = List.map (fun x -> "-I"^(!rootPath^"/"^(x))) options.include_paths in
  let macros_opt = List.map (fun x -> "-D"^x) options.macros in
  (* let macros_opt = if options.strategies then ("-D" ^ options.macroStrategiesFunctionPrefix) :: macros_opt else macros_opt in *)
  let macroStrategiesFunctionPrefix_opt = List.map (fun x -> "-D"^options.macroStrategiesFunctionPrefix^x) options.fct in
  let cpp_extra_args_opt = "-cpp-extra-args=\""^(options.cpp_extra_args)^""^(String.concat "" include_paths_opt)^""^(String.concat "" macros_opt)^""^(String.concat "" macroStrategiesFunctionPrefix_opt)^"\"" in
  let machdep_opt = (option_if_not_empty_string options.machdep "-machdep=") in
  let generated_spec_custom_opt = option_if_not_empty_string (String.concat "," options.generated_spec_custom) "-generated-spec-custom=" in
  let remove_unused_specified_functions_opt = option_if_true options.keep_unused_specified_functions "-remove-unused-specified-functions" in
  let aggressive_merging_opt = option_if_true options.aggressive_merging "-aggressive-merging" in
  let kernel_warn_key_opt = option_if_not_empty_string options.kernel_warn_key "-kernel-warn-key=" in
  let no_unicode_opt = option_if_true options.no_unicode "-no-unicode" in
  let inline_calls_opt = option_if_not_empty_string (String.concat "," options.inline_calls) "-inline-calls=" in
  let remove_inlines_opt = option_if_not_empty_string (String.concat "," options.remove_inlined) "-remove-inlined=" in
  let no_annot_opt = option_if_true options.no_annot "-no-annot" in
  Printf.sprintf "%s %s %s %s %s %s %s %s %s %s"
  cpp_extra_args_opt machdep_opt generated_spec_custom_opt remove_unused_specified_functions_opt aggressive_merging_opt kernel_warn_key_opt no_unicode_opt inline_calls_opt remove_inlines_opt no_annot_opt
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
    wp_par: int;
    wp_session: string;
    wp_smoke_tests: bool;
    wp_smoke_timeout: int;
    wp_script: string;
    wp_cache: string;
    wp_auto_depth: int;
    wp_auto_width: int;
    wp_auto_backtrack: int;
    wp_filename_truncation: int;
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
    wp_par = !Configuration.global_params.wpPar;
    wp_session = !Configuration.global_params.wpSession;
    wp_smoke_tests = (match wp_smoke_tests with None -> false | Some s -> s);
    wp_smoke_timeout = 3;
    wp_script = (match wp_script with | None -> !Configuration.global_params.wpScript | Some s -> s);
    wp_cache = !Configuration.global_params.wpCache;
    wp_auto_depth = !Configuration.global_params.wpAutoDepth;
    wp_auto_width = !Configuration.global_params.wpAutoWidth;
    wp_auto_backtrack = !Configuration.global_params.wpAutoBacktrack;
    wp_filename_truncation = !Configuration.global_params.wpFilenameTruncation;
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let wp_opt = option_if_true options.wp "-wp" in
    let wp_prop_opt = option_if_not_empty_string (String.concat "," options.wp_prop) "-wp-prop=" in
    let wp_fct_opt = option_if_not_empty_string (String.concat "," options.wp_fct) "-wp-fct=" in
    let wp_gen_opt = option_if_true options.wp_gen "-wp-gen" in
    let wp_rte_opt = option_if_true options.wp_rte "-wp-rte" in
    let wp_pruning_opt = option_if_true options.wp_pruning "-wp-no-pruning" in
    let wp_check_memory_model_opt = option_if_true options.wp_check_memory_model "-wp-model \"Typed+var+int+float\" -wp-check-memory-model" in
    let wp_no_volatile_opt = option_if_true options.wp_no_volatile "-wp-no-volatile" in
    let wp_prover_opt = option_if_not_empty_string (String.concat "," options.wp_prover) "-wp-prover=" in
    let wp_timeout_opt = Printf.sprintf "-wp-timeout=%d" options.wp_timeout in
    let wp_par_opt = Printf.sprintf "-wp-par=%d" options.wp_par in
    let wp_session_opt = option_if_not_empty_string options.wp_session "-wp-session=" in
    let wp_smoke_tests_opt = option_if_true options.wp_smoke_tests "-wp-smoke-tests" in
    let wp_smoke_timeout_opt = option_if_not_empty_string (Stdlib.string_of_int options.wp_smoke_timeout) "-wp-smoke-timeout=" in
    let wp_script = option_if_not_empty_string (options.wp_script) "-wp-script=" in
    let wp_cache = option_if_not_empty_string (options.wp_cache) "-wp-cache=" in
    let wp_auto_depth = Printf.sprintf "-wp-auto-depth=%d" options.wp_auto_depth in
    let wp_auto_width = Printf.sprintf "-wp-auto-width=%d" options.wp_auto_width in
    let wp_auto_backtrack = Printf.sprintf "-wp-auto-backtrack=%d" options.wp_auto_backtrack in
    let wp_filename_truncation = Printf.sprintf "-wp-filename-truncation=%d" options.wp_filename_truncation in
    Printf.sprintf "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s" wp_opt wp_prop_opt wp_fct_opt wp_gen_opt wp_rte_opt wp_pruning_opt wp_check_memory_model_opt wp_no_volatile_opt wp_prover_opt wp_timeout_opt wp_par_opt wp_session_opt wp_smoke_tests_opt wp_smoke_timeout_opt wp_script wp_cache wp_auto_depth wp_auto_width wp_auto_backtrack wp_filename_truncation
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
    let meta_warn_key_opt = option_if_not_empty_string options.meta_warn_key "-meta-warn-key=" in
    let meta_checks_opt = option_if_true options.meta_checks "-meta-checks" in
    let meta_no_simpl_opt = option_if_true  options.meta_no_simpl "-meta-no-simpl" in
    let meta_no_check_ext_opt = option_if_true options.meta_no_check_ext "-meta-no-check-ext" in
    let meta_number_assertions_opt = option_if_true options.meta_number_assertions "-meta-number-assertions" in
    let meta_check_callee_assigns_opt = option_if_not_empty_string (String.concat "," options.meta_check_callee_assigns) "-meta-check-callee-assigns=" in
    if options.meta then 
    Printf.sprintf "%s %s %s %s %s %s %s %s" meta_opt meta_warn_key_opt meta_warn_key_opt meta_checks_opt meta_no_simpl_opt meta_no_check_ext_opt meta_number_assertions_opt meta_check_callee_assigns_opt
    else ""
end

module MetricsOpt = struct
  type t = {
    metrics : bool;
    metrics_by_function: bool;
    metrics_output: string;
  }
  let create () = {
    metrics = true;
    metrics_by_function = !Configuration.global_params.metricsByFunction;
    metrics_output = ".frama-c/fc_metrics.txt"
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (opt ^ s) else "" in
    let option_if_true b opt = if b then (opt) else "" in
    let metrics_opt = option_if_true options.metrics "-metrics" in
    let metrics_by_function_opt = option_if_true options.metrics_by_function "-metrics-by-function" in
    let metrics_output_opt = option_if_not_empty_string options.metrics_output "-metrics-output=" in
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
    let print_opt = option_if_true options.print "-print" in
    let no_unicode_opt = option_if_true options.no_unicode "-no-unicode" in
    let ocode_opt = option_if_not_empty_string options.ocode "-ocode=" in
    let no_annot_opt = option_if_true options.no_annot "-no-annot" in
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
  let create ~file () : t = 
    let ofile = Printf.sprintf ".frama-c/fc_%s.dot" file in
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
    | ComputeCallGraph_feature _ -> ""
    | ComputeMetrics_feature -> ""
    | ComputeProofObligation_feature (root_path, id, file, line, column) -> Printf.sprintf "-lsp-root-path=\"%s\" -lsp-id=\"%d\" -lsp-show-povc=%s:%d:%d" root_path id file line column
    | ComputeProofObligationID_feature (id, goal_id) -> Printf.sprintf "-lsp-id=\"%d\" -lsp-show-po=%s" id goal_id
    | Prove_feature (id, file, fct, prop) -> Printf.sprintf "-lsp-id=\"%d\" -lsp-prove=%s:%s:%s" id file fct prop
end


module Command = struct
  type t = {
  wrapper_port: int;
  frama_c_exe: string;
  verbose: int;
  files : string list option;
  kernel : KernelOpt.t option;
  wp : WpOpt.t option;
  metacsl : MetacslOpt.t option;
  metrics : MetricsOpt.t option;
  pprint : PprintOpt.t option;
  cg : CgOpt.t option;
  lsp : LspOpt.t option;
  }
  let create ~port ?gui ?kernel ~strategies ?files ?wp ?metacsl ?metrics ?pprint ?cg ?lsp () : t = {
    wrapper_port = port;
    frama_c_exe = (match gui with None -> "frama-c" | Some g -> if g then "frama-c-gui" else "frama-c");
    verbose = !Configuration.global_params.acslLsp;
    files =
    (match kernel, files with
      | None, _ -> None
      | _, Some fls -> (
        let fls = if strategies then (fls @ !Configuration.global_params.sourceFileStrategies) else fls in
        (match metacsl with
        | None -> Some fls
        | Some _ -> Some (fls @ !Configuration.global_params.sourceFileMetacsl)
        )
      )
      | _, None -> 
        let sourceFiles = List.map (fun x -> (!rootPath)^"/"^x) (!Configuration.global_params.sourceFiles @ !Configuration.global_params.sourceFileStrategies @ !Configuration.global_params.sourceFileMetacsl) in
        match sourceFiles with
        | [] -> Some (Utils.get_workspace_files !rootPath)
        | _ -> Some (sourceFiles)
    );
    kernel = kernel;
    wp = wp;
    metacsl = metacsl;
    metrics = metrics;
    pprint = pprint;
    cg = cg;
    lsp = lsp
  }
  let string_of_t (options : t) : string =
    let option_if_not_empty_string s opt = if not (String.trim s = "") then (s ^ " " ^ opt) else "" in
    let file_names = match options.files with None -> "" | Some f -> String.concat " " f in
    let debug_level = Stdlib.string_of_int options.verbose in
    let common_opt = Printf.sprintf "%s -lsp -lsp-wrapper=%d -lsp-debug=%s %s" options.frama_c_exe options.wrapper_port debug_level file_names in
    let kernel_opt = match options.kernel with None -> "" | Some k -> KernelOpt.string_of_t k in
    let wp_opt = match options.wp with None -> "" | Some w -> option_if_not_empty_string (WpOpt.string_of_t w) "" in
    let metrics_opt = match options.metrics with None -> "" | Some m -> option_if_not_empty_string (MetricsOpt.string_of_t m) "" in
    let pprint_opt = match options.pprint with None -> "" | Some p -> option_if_not_empty_string (PprintOpt.string_of_t p) "" in
    let metacsl_opt = match options.metacsl with None -> "" | Some m -> option_if_not_empty_string (MetacslOpt.string_of_t m) "-then-last" in
    let cg_opt = match options.cg with None -> "" | Some c -> option_if_not_empty_string (CgOpt.string_of_t c) "" in
    let lsp_opt = match options.lsp with None -> "" | Some l -> option_if_not_empty_string (LspOpt.string_of_t l) "" in
    let frama_c_cmd = Printf.sprintf "%s %s %s %s %s %s %s %s" common_opt kernel_opt metacsl_opt wp_opt metrics_opt pprint_opt cg_opt lsp_opt in
    frama_c_cmd
  
  let args_of_t (options : t) : string * string array =
    let replace_all_occurrences pattern replacement input_string =
      let regex = Str.regexp pattern in
      Str.global_replace regex replacement input_string
    in
    let split_and_remove_blank str =
      let words = String.split_on_char ' ' str in
      let lst = List.filter (fun s -> s <> "") words in
      let lst = List.map (replace_all_occurrences "-I" " -I ") lst in
      let lst = List.map (replace_all_occurrences "-D" " -D ") lst in
      let lst = List.map (replace_all_occurrences "\"" "") lst in
      let lst = lst in
      Array.of_list lst
    in
    let cmd = string_of_t options in
    let prog = options.frama_c_exe in
    let args = split_and_remove_blank cmd in
    prog, args
  end



let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

let send_request server_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  Options.Self.debug ~level:1 "Size of response : %d, Response: %s\n%!" (String.length response_str) response_str;
  let response_bytes = Bytes.of_string response_str in
  if ((Bytes.length response_bytes) < 65530) then
    let sent = Unix.send server_sock response_bytes 0 (Bytes.length response_bytes) [] in
    Options.Self.debug ~level:1 "Size of sent content : %d\n%!" sent

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
    Options.Self.debug ~level:1 "\t%s ---- %d\n%!" (filename) pos;
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
    Options.Self.feedback ~level:1 "\t%s\n%!" (msg);
    if (Utils.contains msg ~suffix: "FRAMA-C EXIT CODE: 0") then false
    else if (Utils.contains msg ~suffix: ": fatal error:") then (
      update_diag_data msg Lsp_types.DiagnosticSeverity.Error;
      had_errors_in_channel oc)
    else if (Utils.contains msg ~suffix: ": error:") then (
      update_diag_data msg Lsp_types.DiagnosticSeverity.Error;
      had_errors_in_channel oc)
    else if (Utils.contains msg ~suffix: ": warning:") then (
      update_diag_data msg Lsp_types.DiagnosticSeverity.Warning;
      had_errors_in_channel oc)
    else if (Utils.contains msg ~suffix: ": note:") then (
      update_diag_data msg Lsp_types.DiagnosticSeverity.Warning;
      had_errors_in_channel oc)
    else had_errors_in_channel oc
  with End_of_file -> Options.Self.debug ~level:1 "\n%!"; true


  let read_socket_in_chunks socket chunk_size =
    let buffer = Bytes.create chunk_size in
    let rec read_data accumulated_data =
      let bytes_received = Unix.read socket buffer 0 chunk_size in
      Options.Self.debug ~level:2 "Read chunk size %d: \n%!" bytes_received;
      if bytes_received = 0 then
        accumulated_data  (* Connection closed *)
      else
        let chunk = Bytes.sub_string buffer 0 bytes_received in
        Options.Self.debug ~level:2 "Read chunk %s: \n%!" chunk;
        read_data (accumulated_data ^ chunk)  (* Accumulate the data *)
    in
    read_data ""

let execute_extra_command prog args =
  let env = Unix.environment () in
  let ic, oc, ec = Unix.open_process_args_full prog args env in
  let _ = had_errors_in_channel ic in
  let _ = had_errors_in_channel ec in
  let status  = Unix.close_process_full (ic, oc, ec) in
  status

let execute_command prog args feature wrapper_port =
  let signal_handler signal = if signal = Sys.sigint then () in
  Sys.set_signal Sys.sigint (Sys.Signal_handle signal_handler);
  let wrapper_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind wrapper_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port));
  Unix.listen wrapper_sock 100;
  let env = Unix.environment () in
  let ic, oc, ec = Unix.open_process_args_full prog args env in
  let cpid = Unix.process_full_pid (ic, oc, ec) in
  let signal_handler signal = if signal = Sys.sigint then Unix.kill cpid Sys.sigkill in
  Sys.set_signal Sys.sigint (Sys.Signal_handle signal_handler);
  let _ = had_errors_in_channel ic in
  let _ = had_errors_in_channel ec in
  let status  = Unix.close_process_full (ic, oc, ec) in
  let special_errors = DidSave.StringMap.fold get_notification_list !DidSave.diag_map [] in
  Options.Self.debug ~level:1 "Read ic completed !!!\n%!";
  match status, prog with
  | Unix.WEXITED 0, "frama-c" ->
    (
    match feature with
      | DidSave_feature
      | FindDefinition_feature (_, _, _, _)
      | FindDeclaration_feature (_, _, _, _)
      | ComputeProofObligation_feature (_, _, _, _, _)
      | ComputeProofObligationID_feature (_, _)
      | Prove_feature (_, _, _, _) ->
      Options.Self.debug ~level:1 "Executed frama-c command (frama-c exited normally)\n%!";
      let (plugin_sock, _) = Unix.accept wrapper_sock in
      let _data_size = getnumber (readcontlen plugin_sock) in
      let chunk_size = 65530 in
      let request_str = read_socket_in_chunks plugin_sock chunk_size in
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
      | ComputeMetrics_feature ->
        Options.Self.debug ~level:1 "No Error after executing frama-c command\n%!";
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Successful operation !") () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        Unix.close wrapper_sock;
        data
      | ComputeCallGraph_feature ofile ->
        let prog = "dot" in
        let args = [|"dot"; "-Tpdf"; Printf.sprintf "%s" ofile; "-o"; Printf.sprintf "%s.pdf" ofile|] in
        let _status = execute_extra_command prog args in
        let prog = "evince" in
        let args = [|"evince"; Printf.sprintf "%s.pdf" ofile|] in
        let _status = execute_extra_command prog args in
        Options.Self.debug ~level:1 "No Error after executing frama-c command\n%!";
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Successful operation !") () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        Unix.close wrapper_sock;
        data
    )
  | _, "frama-c-gui" -> (
    match feature with
    | DidSave_feature
    | ComputeCIL_feature
    | ComputeCallGraph_feature _
    | ComputeMetrics_feature ->
      Options.Self.debug ~level:1 "Frama-C GUI ended !! \n%!";
      let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Frama-C GUI ended !!") () in
      let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
      let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
      Unix.close wrapper_sock;
      data
    | FindDefinition_feature (id, _, _, _)
    | FindDeclaration_feature (id, _, _, _)
    | ComputeProofObligation_feature (_, id, _, _, _)
    | ComputeProofObligationID_feature (id, _) ->
      Options.Self.debug ~level:1 "Frama-C GUI ended ! \n%!";
      let lsp_response = Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(`Null) ()) in
      let data = Json.save_string lsp_response in
      Unix.close wrapper_sock;
      data
    | Prove_feature (id, _, _, _) ->
      Options.Self.debug ~level:1 "Frama-C GUI ended ! \n%!";
      let lsp_response = Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(`List [`String ""; `String ""; `List []]) ()) in
      let data = Json.save_string lsp_response in
      Unix.close wrapper_sock;
      data
    )
  | Unix.WEXITED _, _
  | Unix.WSIGNALED _, _
  | Unix.WSTOPPED _, _ ->
    (
      match feature with
      | DidSave_feature ->
          Options.Self.debug ~level:1 "Executed frama-c command (frama-c may have exited with errors)\n%!";
          let (plugin_sock, _) = Unix.accept wrapper_sock in
          let _data_size = getnumber (readcontlen plugin_sock) in
          let chunk_size = 65530 in
          let request_str = read_socket_in_chunks plugin_sock chunk_size in
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
  
      | FindDefinition_feature (id, _, _, _)
      | FindDeclaration_feature (id, _, _, _)
      | ComputeProofObligation_feature (_, id, _, _, _)
      | ComputeProofObligationID_feature (id, _)
      | Prove_feature (id, _, _, _) ->
        Options.Self.debug ~level:1 "\n%!";
        let msg = Printf.sprintf "Frama-c may have exited with errors" in
        let lsp_error_message = Lsp_types.ResponseError.create ~code:(-32603) ~message:msg () in
        let lsp_message = (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~error:lsp_error_message ()) in
        let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in
        Unix.close wrapper_sock;
        data
  
      | ComputeCIL_feature
      | ComputeCallGraph_feature _
      | ComputeMetrics_feature ->
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Error ~message: "Frama-c error ! Check OUTPUT !" () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        Unix.close wrapper_sock;
        data
      )

let fork_execute_command prog args feature wrapper_port =
  let pid = Unix.fork () in
  if not (pid = 0) then (
    fork_pid := pid;
    Options.Self.debug ~level:1 "Request sent to Frama-C !\n%!";
    let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Request sent to Frama-C !") () in
    let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
    let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
    data, pid
  )
  else
    try
      (execute_command prog args feature wrapper_port), pid
    with exn -> (match feature with
      | FindDefinition_feature (id, _, _, _)
      | FindDeclaration_feature (id, _, _, _)
      | ComputeProofObligation_feature (_, id, _, _, _)
      | ComputeProofObligationID_feature (id, _)
      | Prove_feature (id, _, _, _) ->
        Options.Self.debug ~level:1 "Server is busy !:! : %s, %s\n" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
        let lsp_error_message = Lsp_types.ResponseError.create ~code:(-32603) ~message:"Server is busy !:!" () in
        let lsp_message = (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~error:lsp_error_message ()) in
        let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in
        data, pid
      | _ ->
        Options.Self.debug ~level:1 "Server is busy !::! : %s, %s\n" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Error ~message: "Server is busy !::!" () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        data, pid)


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


let check_fct fct =
  let fct = String.trim fct in
  let fct_lst = Str.split (Str.regexp ",") fct in
  if (List.exists (fun element -> element = "@all") fct_lst) then []
  else if (List.exists (fun element -> element = "@axiomatic") fct_lst) then []
  else fct_lst

let check_prop fct prop =
  let prop = String.trim prop in
  let fct = String.trim fct in
  let prop_lst = Str.split (Str.regexp ",") prop in
  let fct_lst = Str.split (Str.regexp ",") fct in
  if (List.exists (fun element -> element = "@all") prop_lst) then []
  else if (List.exists (fun element -> element = "@axiomatic") fct_lst) then "@lemma" :: prop_lst
  else prop_lst

let rq_handler json_string wrapper_port =
  let json = Json.load_string json_string in 
  let request = Lsp_types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  let id = request.id in
  try
    match curr_method with 
    | "initialize" -> 
      Options.Self.debug ~level:1 "initialize\n%!";
      let req_json = (Lsp_types.RequestMessage.json_of_t request) in
      let temp = Utils.remove_newline (Utils.remove_quotes (Json.save_string (Json.field "rootPath" (Json.field "params" req_json)))) in
      rootPath := temp;
      Lsp_types.CONTENT (capabilities_str), 1;
    | "textDocument/definition" -> 
      Options.Self.debug ~level:1 "definition\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.DefinitionParams.t_of_json p
        | None -> Options.Self.debug ~level:1 "No definition params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let src_file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
      let line = params.position.line in 
      let ch = params.position.character in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let feature = FindDefinition_feature ((Utils.id_to_int request.id), src_file, line, ch) in
      let lsp_opt = LspOpt.create (feature) in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT data, pid;
      
    | "textDocument/declaration" -> 
      Options.Self.debug ~level:1 "declaration\n%!";
      let params = match request.params with 
        | Some p -> Lsp_types.DeclarationParams.t_of_json p
        | None -> Options.Self.debug ~level:1 "No declaration params \n%!"; assert false
      in
      let uri = params.textDocument.uri in 
      let src_file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
      let line = params.position.line in 
      let ch = params.position.character in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let feature = FindDeclaration_feature ((Utils.id_to_int request.id), src_file, line, ch) in
      let lsp_opt = LspOpt.create (feature) in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

    | "showPOVC" -> (* show proof obligation of specific function *)
      let id = (Utils.id_to_int request.id) in
      Options.Self.debug ~level:1 "showPOVC, %d\n%!" id;
      let (file, line, ch) = match request.params with 
          | Some `List 
            [`List 
              [`String f; `Assoc [
                "line", `Int l;
                "character", `Int c;
              ]]] -> 
            (Utils.remove_newline (Utils.remove_quotes (f)), l, c)
          | _ -> Options.Self.debug ~level:1 "No params for showPOVC \n%!"; assert false
        in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let wp_opt = WpOpt.create ~wp_prover:["none"] () in
      let feature = ComputeProofObligation_feature (!rootPath, id, file, line, ch) in
      let lsp_opt = LspOpt.create (feature) in
      let command = 
        match (String.ends_with ~suffix:".c" file) with
        | true -> Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~files:[file] ~wp:wp_opt ~lsp:lsp_opt ()
        | false -> Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~wp:wp_opt ~lsp:lsp_opt ()
      in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

      | "showPO" -> (* show proof obligation of specific function *)
      let id = (Utils.id_to_int request.id) in
      Options.Self.debug ~level:1 "showPO, %d\n%!" id;
      let (_file_id, function_id, goal_id) = match request.params with 
          | Some `List [`List [`String file_id; `String function_id; `String goal_id;]] ->
            (Utils.remove_newline (Utils.remove_quotes (file_id)),
             Utils.remove_newline (Utils.remove_quotes (function_id)),
             Utils.remove_newline (Utils.remove_quotes (goal_id)))
          | _ -> Options.Self.debug ~level:1 "No params for showPO \n%!"; assert false
      in
      let function_ids = check_fct function_id in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let wp_opt = WpOpt.create ~wp_fct:function_ids ~wp_prover:["none"] () in
      let feature = ComputeProofObligationID_feature (id, goal_id) in
      let lsp_opt = LspOpt.create (feature) in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~wp:wp_opt ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

      | "provePO" -> (* prove with WP *)
      let id = (Utils.id_to_int request.id) in
      Options.Self.debug ~level:1 "provePO, %d\n%!" id;
      let (file, fct, prop, timeout, gui) = match request.params with
          | Some `List
            [`List 
              [`String f;
              `String function_name;
              `String property_name;
              `Int timeout;
              `Bool gui
              ]] -> 
            (Utils.remove_newline (Utils.remove_quotes (f)), function_name, property_name, timeout, gui)
          | _ -> Options.Self.debug ~level:1 "No params for showPOVC \n%!"; assert false
      in
      let prop = check_prop fct prop in
      let fct = check_fct fct in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let wp_opt = WpOpt.create ~wp_fct:fct ~wp_prop:prop ~wp_gen:false ~wp_timeout:timeout () in
      let metacsl_opt = MetacslOpt.create () in
      let feature = Prove_feature (id, file, (String.concat "," fct), (String.concat ","prop)) in
      let lsp_opt = LspOpt.create (feature) in
      let command = 
        match (String.ends_with ~suffix:".c" file) with
        | true -> Command.create ~port:wrapper_port ~strategies:false ~gui:gui ~kernel:kernel_opt ~files:[file] ~wp:wp_opt ~metacsl:metacsl_opt ~lsp:lsp_opt ()
        | false -> Command.create ~port:wrapper_port ~strategies:false ~gui:gui ~kernel:kernel_opt ~wp:wp_opt ~metacsl:metacsl_opt ~lsp:lsp_opt ()
      in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

      | "provePOStrategies" -> (* prove with WP strategies *)
      let id = (Utils.id_to_int request.id) in
      Options.Self.debug ~level:1 "provePOStrategies, %d\n%!" id;
      let (file, fct, prop, timeout, gui) = match request.params with
          | Some `List
            [`List 
              [`String f;
              `String function_name;
              `String property_name;
              `Int timeout;
              `Bool gui
              ]] -> 
            (Utils.remove_newline (Utils.remove_quotes (f)), function_name, property_name, timeout, gui)
          | _ -> Options.Self.debug ~level:1 "No params for showPOStrategies \n%!"; assert false
        in
      let prop = check_prop fct prop in
      let fct = check_fct fct in
      let kernel_opt = KernelOpt.create ~fct:fct ~strategies:true () in
      let wp_opt = WpOpt.create ~wp_fct:fct ~wp_prop:prop ~wp_prover:["tip"] ~wp_script:"dry" ~wp_gen:false ~wp_timeout:timeout () in
      let metacsl_opt = MetacslOpt.create () in
      let feature = Prove_feature (id, file, (String.concat "," fct), (String.concat "," prop)) in
      let lsp_opt = LspOpt.create (feature) in
      let command = 
        match (String.ends_with ~suffix:".c" file) with
        | true -> Command.create ~port:wrapper_port ~strategies:true ~gui:gui ~kernel:kernel_opt ~files:[file] ~wp:wp_opt ~metacsl:metacsl_opt ~lsp:lsp_opt ()
        | false -> Command.create ~port:wrapper_port ~strategies:true ~gui:gui ~kernel:kernel_opt ~wp:wp_opt ~metacsl:metacsl_opt ~lsp:lsp_opt ()
      in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

    | "stop" -> (
        Options.Self.debug ~level:1 "Kill pid %d!%!" !fork_pid;
        if not (!fork_pid = 0) then Unix.kill !fork_pid Sys.sigint;
        fork_pid := 0;
        let lsp_response = Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:request.id ~result:`Null ()) in
        Lsp_types.CONTENT (Json.save_string lsp_response), 1;
    )
    | "shutdown" -> receivedShutdown := true;
      let lsp_response = Lsp_types.ResponseMessage.json_of_t (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:request.id ~result:`Null ()) in
      Lsp_types.CONTENT (Json.save_string lsp_response), 1;
    | _ -> 
      Lsp_types.CONTENT (Json.save_string `Null), 1
  with exn ->  
    Options.Self.debug ~level:1 "Backtrace : %s\n" (Printexc.get_backtrace ());
    Lsp_types.CONTENT (Json.save_string (Utils.make_error (Printexc.to_string (exn)) (Utils.id_to_int id))), 1


let notif_handler json_string server_sock wrapper_port =
  let json = Json.load_string json_string in 
  let notif = Lsp_types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    Options.Self.debug ~level:1 "initialized\n%!";
    send_request server_sock (Json.save_string Configuration.request_configurations);
    let registrations = [Lsp_types.Registration.create ~id:"registration" ~method_:"workspace/didChangeConfiguration" ()] in
    let lsp_registration_param = Lsp_types.RegistrationParams.create ~registrations:registrations () in
    let json_registration_params = Lsp_types.RegistrationParams.json_of_t lsp_registration_param in
    let lsp_request = Lsp_types.RequestMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Str "register_capability") ~method_:"client/registerCapability" ~params:json_registration_params () in
    let json_request = Lsp_types.RequestMessage.json_of_t lsp_request in
    let data = Json.save_string (json_request) in
    Lsp_types.CONTENT (data), 1

  | "textDocument/didSave" ->
    Options.Self.debug ~level:1 "didSave\n%!";
    let params = match notif.params with
      | Some p -> Lsp_types.DidSaveTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in
    let file_name = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    if String.ends_with ~suffix:".c" file_name then
      begin
        let kernel_opt = KernelOpt.create ~strategies:false () in
        let wp_opt = WpOpt.create ~wp_prop:["@assigns"; "rte"] ~wp_prover:["none"] ~wp_smoke_tests:false ~wp_gen:true () in
        let metacsl_opt = MetacslOpt.create () in
        let feature = DidSave_feature in
        let lsp_opt = LspOpt.create (feature) in
        let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~files:[file_name] ~wp:wp_opt ~metacsl:metacsl_opt ~lsp:lsp_opt () in
        let command_str = (Command.string_of_t command) in
        Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
        let prog, args = (Command.args_of_t command) in
        let data = execute_command prog args feature wrapper_port in
        Lsp_types.CONTENT (data), 1;
      end
    else Lsp_types.EMPTY (), 1

  | "showGlobalMetrics" -> 
    Options.Self.debug ~level:1 "global metrics\n%!";
    let kernel_opt = KernelOpt.create ~strategies:false () in
    let metrics_opt = MetricsOpt.create () in
    let feature = ComputeMetrics_feature in
    let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~metrics:metrics_opt () in
    let command_str = (Command.string_of_t command) in
    Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
    let prog, args = (Command.args_of_t command) in
    let data, pid = fork_execute_command prog args feature wrapper_port in
    Lsp_types.CONTENT (data), pid;

  | "smokeTests" -> 
      Options.Self.debug ~level:1 "smokeTests\n%!";
      let file_name = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Options.Self.debug ~level:1 "No params for smokeTests \n%!"; assert false
      in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let wp_opt = WpOpt.create ~wp_prop:["smoke"] ~wp_smoke_tests:true ~wp_gen:false () in
      let feature = DidSave_feature in
      let lsp_opt = LspOpt.create (feature) in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~files:[file_name] ~wp:wp_opt ~lsp:lsp_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data = execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), 1;

  | "displayCIL" -> 
      Options.Self.debug ~level:1 "displayCIL\n%!";
      let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Options.Self.debug ~level:1 "No params for displayCIL \n%!"; assert false
      in
      let feature = ComputeCIL_feature in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let file_basename = (Filename.remove_extension (Filename.basename (String.trim file))) in
      let pprint_opt = PprintOpt.create ~file:file_basename () in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~files:[file] ~pprint:pprint_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

  | "displayCILProject" -> 
      Options.Self.debug ~level:1 "displayCILProject\n%!";
      let feature = ComputeCIL_feature in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let file_basename = "project" in
      let pprint_opt = PprintOpt.create ~file:file_basename () in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~pprint:pprint_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

  | "displayCIL_noannot" -> 
      Options.Self.debug ~level:1 "displayCIL_noannot\n%!";
      let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Options.Self.debug ~level:1 "No params for displayCIL \n%!"; assert false
      in
      let feature = ComputeCIL_feature in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let file_basename = (Filename.remove_extension (Filename.basename (String.trim file))) in
      let pprint_opt = PprintOpt.create ~file:file_basename ~no_annot:true () in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~files:[file] ~pprint:pprint_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;

  | "displayCILProject_noannot" -> 
      Options.Self.debug ~level:1 "displayCILProject_noannot\n%!";
      let feature = ComputeCIL_feature in
      let kernel_opt = KernelOpt.create ~strategies:false () in
      let file_basename = "project" in
      let pprint_opt = PprintOpt.create ~file:file_basename ~no_annot:true () in
      let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~pprint:pprint_opt () in
      let command_str = (Command.string_of_t command) in
      Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
      let prog, args = (Command.args_of_t command) in
      let data, pid = fork_execute_command prog args feature wrapper_port in
      Lsp_types.CONTENT (data), pid;
  
  | "showLocalMetrics" -> 
    Options.Self.debug ~level:1 "local metrics\n%!";
    let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Options.Self.debug ~level:1 "No params for metrics \n%!"; assert false
    in
    let feature = ComputeMetrics_feature in
    let kernel_opt = KernelOpt.create ~strategies:false () in
    let metrics_opt = MetricsOpt.create () in
    let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~files:[file] ~metrics:metrics_opt () in
    let command_str = (Command.string_of_t command) in
    Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
    let prog, args = (Command.args_of_t command) in
    let data, pid = fork_execute_command prog args feature wrapper_port in
    Lsp_types.CONTENT (data), pid
  
  | "computeCG" -> 
    Options.Self.debug ~level:1 "computeCG\n%!";
    let file = match notif.params with 
        | Some `List [f] -> Utils.remove_newline (Utils.remove_quotes (Json.save_string f))
        | _ -> Options.Self.debug ~level:1 "No params for computeCG \n%!"; assert false
    in
  
    let file_basename = (Filename.remove_extension (Filename.basename (String.trim file))) in
    let kernel_opt = KernelOpt.create ~strategies:false () in
    let cg_opt = CgOpt.create ~file:file_basename () in
    let command = Command.create ~port:wrapper_port ~strategies:false ~kernel:kernel_opt ~files:[file] ~cg:cg_opt () in
    let feature = ComputeCallGraph_feature cg_opt.cg in
    let command_str = (Command.string_of_t command) in
    Options.Self.feedback ~level:1 "Command = %s\n%!" command_str;
    let prog, args = (Command.args_of_t command) in
    let data, pid = fork_execute_command prog args feature wrapper_port in
    Lsp_types.CONTENT (data), pid

  | "workspace/didChangeConfiguration" ->
    Options.Self.debug ~level:1 "didChangeConfiguration\n%!";
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
    | None -> Options.Self.debug ~level:1 "No result \n%!"; assert false
  in
  let id = request.id in
  match id with
  | Lsp_types.Str "ask_configs" -> (* if the result is request_configurations *)
    Configuration.save_configs (result);
    Lsp_types.EMPTY (), 1;
  | _ -> 
    Lsp_types.EMPTY (), 1

let error_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Lsp_types.ResponseMessage.t_of_json json in 
  let error = 
    match request.error with 
    | Some err -> err 
    | None -> Options.Self.debug ~level:1 "No error \n%!"; assert false
  in 
  let data = Json.save_string (Lsp_types.ResponseError.json_of_t (error)) in
  Lsp_types.CONTENT (data), 1

exception UnknownRequest of int

let handle (json_string : string) server_sock wrapper_port : (Lsp_types.lsp_result * int) =
  let root_dir = ".frama-c" in
  if not (Sys.file_exists root_dir) then Unix.mkdir root_dir 0o755;
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      if (List.exists (fun (key, _) -> key = "result") fields) then
        begin
          Options.Self.debug ~level:1 "result_handler\n%!";
          result_handler json_string 
        end
      else if (List.exists (fun (key, _) -> key = "error") fields) then 
        begin
          Options.Self.debug ~level:1 "error_handler\n%!";
          error_handler json_string
        end
      else if (not (List.exists (fun (key, _) -> key = "id") fields)) then 
        begin
          Options.Self.debug ~level:1 "notif_handler\n%!";
          notif_handler json_string server_sock wrapper_port
        end
      else if (List.exists (fun (key, _) -> key = "id") fields) then 
        begin
          Options.Self.debug ~level:1 "rq_handler\n%!";
          rq_handler json_string wrapper_port
        end
      else
        raise (UnknownRequest 1)
    | _ -> Options.Self.debug ~level:1 "no result\n%!"; raise (UnknownRequest 1)
  with
  | Json.Error _ -> raise (UnknownRequest 1)
    
