let global_params = ref Frama_c_kernel.Json.(`List [])
let () = Printf.printf "global_params length : %d\n%!" (List.length (Frama_c_kernel.Json.list !global_params))

let request_configurations : Frama_c_kernel.Json.json = 
  
  Acsl_lsp.Settings.Self.debug ~level:1 "Asking for configurations\n%!";
  Acsl_lsp.Types.RequestMessage.json_of_t 
  (Acsl_lsp.Types.RequestMessage.create 
    ~jsonrpc:"2.0" 
    ~id:(Acsl_lsp.Types.Int (123456789)) 
    ~method_:"workspace/configuration" 
    ~params:( Frama_c_kernel.Json.load_string 
    {|
      {
        "items":
        [
          {"section": "vscodeacsl.trace.acslLsp"},
          {"section": "kernel.includePaths"},
          {"section": "kernel.sourceFiles"},
          {"section": "kernel.macros"},
          {"section": "kernel.cc"},
          {"section": "kernel.cppCommand"},
          {"section": "kernel.machdep"},
          {"section": "kernel.cppGnuLike"},
          {"section": "kernel.framacStdlib"},
          {"section": "kernel.keepUnusedSpecifiedFunctions"},
          {"section": "kernel.keepUnusedTypes"},
          {"section": "kernel.aggressiveMerging"},
          {"section": "kernel.generatedSpecCustom"},
          {"section": "kernel.continueAnnotError"},
          {"section": "kernel.origName"},
          {"section": "kernel.print"},
          {"section": "kernel.annot"},
          {"section": "kernel.keepComments"},
          {"section": "kernel.kernelLog"},
          {"section": "kernel.ocode"},
          {"section": "wp.wpPruning"},
          {"section": "metrics.metrics"},
          {"section": "callgraph.cg"},
          {"section": "callgraph.cgRoots"},
          {"section": "callgraph.cgServices"},
          {"section": "wp.wp"}
        ]
      }
    |})
    ())
  
let save_configs (result:  Frama_c_kernel.Json.json) = 
  (* note : result arguments must be in the same order as in the configuration request *)
  match result with
  | `List _
    -> 
      global_params := result;
      Printf.printf "save_configs : global_params length : %d\n%!" (List.length (Frama_c_kernel.Json.list !global_params))
  | _ -> 
    Acsl_lsp.Settings.Self.debug ~level:1 "Requested unknown configuration(s), error : \n\t%s\n%!" (Printexc.get_backtrace ())

