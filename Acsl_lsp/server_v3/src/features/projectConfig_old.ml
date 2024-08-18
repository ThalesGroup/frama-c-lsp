let acsl_debug = ref 3
let includePaths = (ref []) 
let sourceFiles = (ref []) 
let macros = (ref []) 
let cc = ref false
let cppCommand = (ref "") 
let machdep = (ref "") 
let cppGnuLike = (ref true)
let framacStdlib = (ref true)
let keepUnusedSpecifiedFunctions = (ref true)
let keepUnusedTypes = (ref true)
let aggressiveMerging = (ref false)
let generatedSpecCustom : (string * string option) list ref = (ref []) 
let continueAnnotError = ref false
let origName = ref false
let print = ref false
let annot = ref false
let keepComments = ref false
let kernelLog = ref ""
let ocode = ref ""
let wpPruning = ref true
let metrics = ref false
let cg = ref ""
let cgRoots = ref []
let cgServices = ref true
let wp = ref false
let validate_includePath path = 
  Filepath.exists (Filepath.Normalized.of_string (Utils.remove_newline (Utils.remove_quotes path))) 

let validate_sourceFile path = 
  Filepath.exists (Filepath.Normalized.of_string (Utils.remove_newline (Utils.remove_quotes path)))
  && 
  String.ends_with ~suffix:".c" (Utils.remove_newline (Utils.remove_quotes path))

let get_acsl_debug () : int = 
  !acsl_debug

let get_includePaths () : string list =
  let existing_dirs = List.filter (fun path -> 
      validate_includePath path
  ) !includePaths in    
  List.map (fun path -> 
    (" -I"^(path))
    ) existing_dirs

let validate_macro macr =
      ignore macr; true
  (* let regex = Str.regexp {|([0-9a-zA-Z_]+(=[0-9a-zA-Z_]*)+)|} in  *)
  (* Str.string_match regex macr 0 *)
  (* todo : write proper validation function with regex *)

let get_macros (): string list = 
  let macros = List.filter (fun macro -> 
      validate_macro macro
    ) !macros in    
  List.map (fun macr -> 
    (" -D"^(macr))
    ) macros
    
let get_cc (): string = 
  if !cc then " -CC" else ""
  
(* note : never forget default behavior of string options if not specified *)    
let get_cppCommand (): string = (* todo : there is no string checking, the command could cause errors *)
  if (String.equal !cppCommand "") 
    then Kernel.CppCommand.get_default ()
  else 
    !cppCommand

let get_machdep (): string = (* todo : there is no string checking, the machdep could cause errors *)
  if (String.equal !machdep "") 
    then Kernel.Machdep.get_default ()
  else 
    !machdep

let get_cppGnuLike (): bool = 
  !cppGnuLike

let get_framacStdlib (): bool = 
  !framacStdlib

let get_keepUnusedSpecifiedFunctions (): bool = 
  !keepUnusedSpecifiedFunctions

let get_keepUnusedTypes (): bool = 
  !keepUnusedTypes

let get_aggressiveMerging (): bool = 
  !aggressiveMerging

let get_origName (): bool = 
  !origName

let get_generatedSpecCustom (): (string * string option) list = 
  !generatedSpecCustom

let get_continueAnnotError () = 
  !continueAnnotError

let get_print (): bool = 
  !print

let get_annot (): bool = 
  !annot

let get_keepComments (): bool = 
  !keepComments

let get_kernelLog (): string = 
  !kernelLog

let get_ocode (): string = 
  !ocode

let get_wpPruning (): bool = 
  !wpPruning

let get_metrics (): bool = 
  !metrics

let get_cg (): string = 
  !cg

let get_cgRoots (): string list = 
  !cgRoots

let get_cgServices (): bool = 
  !cgServices

let get_wp (): bool = 
  !wp

let set_generatedSpecCustom () = 
  if not ((List.length !generatedSpecCustom) = 0) then 
  List.iter (fun (k, v) ->
    Kernel.GeneratedSpecCustom.add (k, v);
  ) !generatedSpecCustom 

let set_continueAnnotError () = 
  if (get_continueAnnotError ()) then
    Kernel.set_warn_status (Kernel.wkey_annot_error) (Log.Winactive)
  else 
    Kernel.set_warn_status (Kernel.wkey_annot_error) (Log.Wactive)

let set_kernelLog () = 
  if not (String.equal !kernelLog "") then
    begin
      let oc = Stdlib.open_out !kernelLog in
      Log.set_output (Stdlib.output_substring oc) (fun () -> Stdlib.flush oc)
    end

let set_ocode () = 
  if not (String.equal !ocode "") then
    Kernel.CodeOutput.set (Filepath.Normalized.of_string !ocode) 
    
let set_cg () = 
  if not (String.equal !cg "") then
    Callgraph.Options.Filename.set (Filepath.Normalized.of_string !cg) 
  else
    Callgraph.Options.Filename.set (Filepath.Normalized.of_string "untitledCallgraph.dot")

let set_cgRoots () = ()
  (* let fxs = Globals.FileIndex.get_functions (Filepath.Normalized.of_string "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/test_files/test1.c") in
  List.iter (fun x ->
    Settings.Self.debug ~level:0 "Kernel function : %s\n%!" (Pretty_utils.to_string (Cil_types_debug.pp_kernel_function) x);
  ) fxs *)
  (* let kf_list = List.map (fun x ->
    try
      Globals.Functions.find_by_name x
    with Not_found -> assert false
  ) !cgRoots in
  Callgraph.Options.Roots.set (Cil_datatype.Kf.Set.of_list kf_list) *)
let save_configs (params: Json.json) = 
  (* note : result arguments must be in the same order as in the configuration request *)
  match params with
  | `List [
      `Int json_acsl_debug;
      `List json_incl; 
      `List json_src; 
      `List json_macr; 
      `Bool json_cc;
      `String json_cppcmd; 
      `String json_machdep; 
      `Bool json_cppGnuLike; 
      `Bool json_framacStdlib;
      `Bool json_keepUnusedSpecifiedFunctions;
      `Bool json_keepUnusedTypes;
      `Bool json_aggressiveMerging;
      `List json_generatedSpecCustom;
      `Bool json_origName;
      `Bool json_print;
      `Bool json_annot;
      `Bool json_keepComments;
      `String json_kernelLog;
      `Bool json_wpPruning;
      `Bool json_metrics;
      `String json_cg;
      `List json_cgRoots;
      `Bool json_cgServices;
      `Bool json_wp
      ] 
    -> 
      acsl_debug := json_acsl_debug;
      cppCommand := (Utils.remove_newline (Utils.remove_quotes (json_cppcmd))) ;
      machdep := (Utils.remove_newline (Utils.remove_quotes (json_machdep))) ;
      cc := json_cc;
      cppGnuLike := json_cppGnuLike;
      framacStdlib := json_framacStdlib;
      keepUnusedSpecifiedFunctions := json_keepUnusedSpecifiedFunctions;
      keepUnusedTypes := json_keepUnusedTypes;
      aggressiveMerging := json_aggressiveMerging;
      origName := json_origName;
      print := json_print;
      annot := json_annot;
      keepComments := json_keepComments;
      kernelLog := (Utils.remove_newline (Utils.remove_quotes (json_kernelLog)));
      wpPruning := json_wpPruning;
      metrics := json_metrics;
      cg := (Utils.remove_newline (Utils.remove_quotes (json_cg)));
      cgServices := json_cgServices;
      wp := json_wp;
  
      (* save include paths *)
      (* includePaths := List.map (fun x ->
        ((Utils.remove_newline (Utils.remove_quotes (Json.save_string x))))
      ) json_incl;  *)
      Kernel.CppExtraArgs.set (List.map (fun x ->
        ("-I"^(Utils.remove_newline (Utils.remove_quotes (Json.save_string x))))
      ) json_incl); 
  
      (* save source files *)
      (* note : List.filter ? *)
      sourceFiles := [];
      List.iter (fun x ->
        let y = (Utils.remove_newline (Utils.remove_quotes (Json.save_string x))) in
        if validate_sourceFile y then 
          begin
            sourceFiles := y :: !sourceFiles;
          end
          else
          sourceFiles := !sourceFiles;
      ) json_src;
  
      (* save macros *)
      macros := List.map (fun x ->
        ((Utils.remove_newline (Utils.remove_quotes (Json.save_string x))))
      ) json_macr;
  
      generatedSpecCustom := Utils.split_key_value (List.map (fun x ->
        ((Utils.remove_newline (Utils.remove_quotes (Json.save_string x))))
      ) json_generatedSpecCustom);

      cgRoots := List.map (fun x ->
        ((Utils.remove_newline (Utils.remove_quotes (Json.save_string x))))
      ) json_cgRoots;
  
  | _ -> 
    Settings.Self.debug ~level:1 "Requested unknown configuration(s), error : \n\t%s\n%!" (Printexc.get_backtrace ())

let set_framac_options params : unit = 
  save_configs params;
  (* note : developer defined settings *)
  let framac_share = Utils.file_str Fc_config.datadir in
  Kernel.Share.set (Fc_config.datadir);
  let share = Kernel.Share.get () in
  Filepath.add_symbolic_dir framac_share share; 
  Filepath.reset_symbolic_dirs ();
  Kernel.Unicode.set(false);

  (* Settings.Self.debug ~level:0 "frama c share : %s\n%!" (Filepath.basename (Kernel.Share.get ())); *)

  (* Kernel.Debug.set (0); *)
  (* note : user defined settings *)
  (* Kernel.CppExtraArgs.set ((["dummy"]) @ (get_macros ()) @ [get_cc ()]); *)
  Kernel.CppCommand.set (get_cppCommand ());
  Kernel.Machdep.set (get_machdep ());
  Kernel.CppGnuLike.set (get_cppGnuLike ());
  Kernel.FramaCStdLib.set (get_framacStdlib ());
  Kernel.Keep_unused_specified_functions.set (get_keepUnusedSpecifiedFunctions ());
  (* Kernel.Keep_unused_types.set (get_keepUnusedTypes ()); *)
  Kernel.AggressiveMerging.set (get_aggressiveMerging ());
  set_generatedSpecCustom ();
  set_continueAnnotError ();
  Kernel.Orig_name.set (get_origName ());
  Kernel.PrintCode.set (get_print ());
  (* Kernel.ReadAnnot.set (get_annot ()); *) (* todo : this option gives an error *)
  Kernel.PrintComments.set (get_keepComments ());
  set_kernelLog ();
  (* set_ocode (); todo : remove *)
  (* Dynamic.Parameter.Bool.set "-metrics" true; *)

  set_cg ();
  set_cgRoots ();
  Callgraph.Options.Services.set (get_cgServices ());

  Wp.Wp_parameters.WP.set (get_wp ()); (* note : the following wp options are set only if -wp is enabled *)
  if (Wp.Wp_parameters.WP.get ()) then
    begin
      (* Settings.Self.debug ~level:0 "QED : %s\n%!" (Pretty_utils.to_string Wp.VCS.pp_prover Wp.VCS.Qed);
      Settings.Self.debug ~level:0 "TACTICAL : %s\n%!" (Pretty_utils.to_string Wp.VCS.pp_prover Wp.VCS.Tactical);
      List.iter (fun x ->
        Settings.Self.debug ~level:0 "WHY 3 : %s\n%!" (Pretty_utils.to_string Wp.VCS.pp_prover (Wp.VCS.Why3 x))
      ) (Wp.Why3Provers.provers ()); *)

      Wp.Wp_parameters.Prune.set (get_wpPruning ());

      (* Wp.Wpo.iter_on_goals (fun x ->
        Settings.Self.debug ~level:0 "Qed result : %s\n%!" (Pretty_utils.to_string Wp.VCS.pp_result (Wp.Wpo.get_result x Wp.VCS.Tactical))
      ); *)

    end;

    Settings.Self.debug ~level:0 "Cpp extra args Configuration\n%!";
    List.iter (fun x ->
      Settings.Self.debug ~level:0 "\t %s\n%!" (x);
    ) (get_includePaths ());