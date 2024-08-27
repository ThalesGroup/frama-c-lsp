let sections = {| {"items": [ {"section": "acslLsp"}, 
          {"section": "kernel.includePaths"},
          {"section": "kernel.sourceFiles"},
          {"section": "kernel.macros"},
          {"section": "kernel.machdep"},
          {"section": "kernel.keepUnusedSpecifiedFunctions"},
          {"section": "kernel.aggressiveMerging"},
          {"section": "kernel.generatedSpecCustom"},
          {"section": "metrics.output"},
          {"section": "callgraph.output"},
          {"section": "callgraph.roots"},
          {"section": "callgraph.services"},
          {"section": "wp.pruning"},
          {"section": "wp.rte"},
          {"section": "wp.checkMemoryModel"},
          {"section": "wp.volatile"},
          {"section": "wp.prover"},
          {"section": "wp.timeout"},
          {"section": "wp.session"},
          {"section": "diagnostics.wp"}
        ]
      }
    |}

  type t = {
    acslLsp : int;
    includePaths : string list;
    sourceFiles : string list;
    macros : string list;
    machdep : string;
    keepUnusedSpecifiedFunctions : bool;
    aggressiveMerging : bool;
    generatedSpecCustom : string list;
    metricsOutput : string;
    cgOutput : string;
    cgRoots : string list;
    cgServices : bool;
    wpPruning : bool;
    wpRte : bool;
    wpCheckMemoryModel : bool;
    wpVolatile : bool;
    wpProver : string;
    wpTimeout : int;
    wpSession : string;
    diagnosticsWp : bool;
  }

  let create
    ~acslLsp 
    ~includePaths
    ~sourceFiles
    ~macros
    ~machdep 
    ~keepUnusedSpecifiedFunctions 
    ~aggressiveMerging 
    ~generatedSpecCustom
    ~metricsOutput 
    ~cgOutput 
    ~cgRoots
    ~cgServices 
    ~wpPruning 
    ~wpRte 
    ~wpCheckMemoryModel 
    ~wpVolatile 
    ~wpProver 
    ~wpTimeout 
    ~wpSession 
    ~diagnosticsWp
    ()
    =
    {
      acslLsp;
      includePaths;
      sourceFiles;
      macros;
      machdep;
      keepUnusedSpecifiedFunctions;
      aggressiveMerging;
      generatedSpecCustom;
      metricsOutput;
      cgOutput;
      cgRoots;
      cgServices;
      wpPruning;
      wpRte;
      wpCheckMemoryModel;
      wpVolatile;
      wpProver; 
      wpTimeout; 
      wpSession; 
      diagnosticsWp;
    }

let global_params = ref
  (create 
  ~acslLsp:2
  ~includePaths:[]
  ~sourceFiles:[]
  ~macros:[]
  ~machdep:""
  ~keepUnusedSpecifiedFunctions:false
  ~aggressiveMerging:false
  ~generatedSpecCustom:[]
  ~metricsOutput:""
  ~cgOutput:""
  ~cgRoots:[]
  ~cgServices:false
  ~wpPruning:false
  ~wpRte:false
  ~wpCheckMemoryModel:false
  ~wpVolatile:false
  ~wpProver: ""
  ~wpTimeout: 2
  ~wpSession: ""
  ~diagnosticsWp: false
  ())

let request_configurations : Json.json = 
  
  Lsp_types.RequestMessage.json_of_t 
  (Lsp_types.RequestMessage.create 
    ~jsonrpc:"2.0" 
    ~id:(Lsp_types.Str ("ask_configs")) 
    ~method_:"workspace/configuration" 
    ~params:( Json.load_string sections)
    ())
  
let save_configs (result:  Json.json) = 
  (* note : result arguments must be in the same order as in the configuration request *)
  match result with
  | `List [
        `Int json_acslLsp; 
        `List json_includePaths; 
        `List json_sourceFiles; 
        `List json_macros; 
        `String json_machdep;
        `Bool json_keepUnusedSpecifiedFunctions;
        `Bool json_aggressiveMerging;
        `List json_generatedSpecCustom; 
        `String json_metricsOutput;
        `String json_cgOutput;
        `List json_cgRoots; 
        `Bool json_cgServices;
        `Bool json_wpPruning;
        `Bool json_wpRte;
        `Bool json_wpCheckMemoryModel;
        `Bool json_wpVolatile;
        `String json_wpProver;
        `Int json_wpTimeout;
        `String json_wpSession;
        `Bool json_diagnosticsWp;
      ] 
    -> 
      global_params := create 
      ~acslLsp: json_acslLsp
      ~includePaths: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_includePaths )
      ~sourceFiles: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFiles)
      ~macros: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_macros)
      ~machdep: (Utils.remove_newline (Utils.remove_quotes (json_machdep)))
      ~keepUnusedSpecifiedFunctions: json_keepUnusedSpecifiedFunctions
      ~aggressiveMerging: json_aggressiveMerging
      ~generatedSpecCustom: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_generatedSpecCustom)
      ~metricsOutput: (Utils.remove_newline (Utils.remove_quotes (json_metricsOutput)))
      ~cgOutput: (Utils.remove_newline (Utils.remove_quotes (json_cgOutput)))
      ~cgRoots: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_cgRoots)
      ~cgServices: json_cgServices
      ~wpPruning: json_wpPruning
      ~wpRte: json_wpRte
      ~wpCheckMemoryModel: json_wpCheckMemoryModel
      ~wpVolatile: json_wpVolatile
      ~wpProver: json_wpProver
      ~wpTimeout: json_wpTimeout
      ~wpSession: json_wpSession
      ~diagnosticsWp: json_diagnosticsWp
      ();


      Lsp.Self.debug ~level:4 "save_configs : global_params length : %d\n%!" (List.length (Json.list result))
  | x -> 
    Lsp.Self.debug ~level:3 "Requested unknown configuration(s), error : %s\n\t%s\n%!" (Json.save_string ~pretty:true x) (Printexc.get_backtrace ())
