let sections = {| {"items": [ {"section": "vscodeacsl.trace.acslLsp"}, 
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
          {"section": "kernel.origName"},
          {"section": "kernel.print"},
          {"section": "kernel.annot"},
          {"section": "kernel.keepComments"},
          {"section": "kernel.kernelLog"},
          {"section": "wp.wpPruning"},
          {"section": "metrics.byFunction"},
          {"section": "metrics.output"},
          {"section": "callgraph.cg"},
          {"section": "callgraph.cgRoots"},
          {"section": "callgraph.cgServices"},
          {"section": "wp.wpRte"},
          {"section": "wp.checkMemoryModel"},
          {"section": "wp.volatile"},
          {"section": "wp.gen"},
          {"section": "wp.dump"},
          {"section": "wp.status"},
          {"section": "wp.smokeTests"}
        ]
      }
    |}

  type t = {
    acslDebug : int;
    includePaths : string list;
    sourceFiles : string list;
    macros : string list;
    cc : bool;
    cppCommand : string;
    machdep : string;
    cppGnuLike : bool;
    framacStdlib : bool;
    keepUnusedSpecifiedFunctions : bool;
    keepUnusedTypes : bool;
    aggressiveMerging : bool;
    generatedSpecCustom : string list;
    origName : bool;
    print : bool;
    annot : bool;
    keepComments : bool;
    kernelLog : string;
    wpPruning : bool;
    metricsByFunction : bool;
    metricsOutput : string;
    cg : string;
    cgRoots : string list;
    cgServices : bool;
    wpRte : bool;
    wpCheckMemoryModel : bool;
    wpVolatile : bool;
    wpGen : bool;
    wpDump : bool;
    wpStatus : bool;
    wpSmokeTests : bool
  }

  let create
    ~acslDebug
    ~includePaths
    ~sourceFiles
    ~macros
    ~cc
    ~cppCommand
    ~machdep
    ~cppGnuLike
    ~framacStdlib
    ~keepUnusedSpecifiedFunctions
    ~keepUnusedTypes
    ~aggressiveMerging
    ~generatedSpecCustom
    ~origName
    ~print
    ~annot
    ~keepComments
    ~kernelLog
    ~wpPruning
    ~metricsByFunction
    ~metricsOutput
    ~cg
    ~cgRoots
    ~cgServices
    ~wpRte
    ~wpCheckMemoryModel
    ~wpVolatile
    ~wpGen
    ~wpDump
    ~wpStatus
    ~wpSmokeTests 
    ()
    =
    {
      acslDebug;
      includePaths;
      sourceFiles;
      macros;
      cc;
      cppCommand;
      machdep;
      cppGnuLike;
      framacStdlib;
      keepUnusedSpecifiedFunctions;
      keepUnusedTypes;
      aggressiveMerging;
      generatedSpecCustom;
      origName;
      print;
      annot;
      keepComments;
      kernelLog;
      wpPruning;
      metricsByFunction;
      metricsOutput;
      cg;
      cgRoots;
      cgServices;
      wpRte;
      wpCheckMemoryModel;
      wpVolatile;
      wpGen;
      wpDump;
      wpStatus;
      wpSmokeTests;
    }

let global_params = ref
  (create 
  ~acslDebug:0
  ~includePaths:[]
  ~sourceFiles:[]
  ~macros:[]
  ~cc:false
  ~cppCommand:""
  ~machdep:""
  ~cppGnuLike:false
  ~framacStdlib:false
  ~keepUnusedSpecifiedFunctions:false
  ~keepUnusedTypes:false
  ~aggressiveMerging:false
  ~generatedSpecCustom:[]
  ~origName:false
  ~print:false
  ~annot:false
  ~keepComments:false
  ~kernelLog:""
  ~wpPruning:false
  ~metricsByFunction:false
  ~metricsOutput:""
  ~cg:""
  ~cgRoots:[]
  ~cgServices:false
  ~wpRte:false
  ~wpCheckMemoryModel:false
  ~wpVolatile:false
  ~wpGen:false
  ~wpDump:false
  ~wpStatus:false
  ~wpSmokeTests:false
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
        `Int json_acslDebug; 
        `List json_includePaths; 
        `List json_sourceFiles; 
        `List json_macros; 
        `Bool json_cc;
        `String json_cppCommand;
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
        `Bool json_metricsByFunction;
        `String json_metricsOutput;
        `String json_cg;
        `List json_cgRoots; 
        `Bool json_cgServices;
        `Bool json_wpRte;
        `Bool json_wpCheckMemoryModel;
        `Bool json_wpVolatile;
        `Bool json_wpGen;
        `Bool json_wpDump;
        `Bool json_wpStatus;
        `Bool json_wpSmokeTests
      ] 
    -> 
      global_params := create 
      ~acslDebug: json_acslDebug
      ~includePaths: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_includePaths )
      ~sourceFiles: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFiles)
      ~macros: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_macros)
      ~cc: json_cc
      ~cppCommand: (Utils.remove_newline (Utils.remove_quotes (json_cppCommand)))
      ~machdep: (Utils.remove_newline (Utils.remove_quotes (json_machdep)))
      ~cppGnuLike: json_cppGnuLike
      ~framacStdlib: json_framacStdlib
      ~keepUnusedSpecifiedFunctions: json_keepUnusedSpecifiedFunctions
      ~keepUnusedTypes: json_keepUnusedTypes
      ~aggressiveMerging: json_aggressiveMerging
      ~generatedSpecCustom: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_generatedSpecCustom)
      ~origName: json_origName
      ~print: json_print
      ~annot: json_annot
      ~keepComments: json_keepComments
      ~kernelLog: (Utils.remove_newline (Utils.remove_quotes (json_kernelLog)))
      ~wpPruning: json_wpPruning
      ~metricsByFunction: json_metricsByFunction
      ~metricsOutput: (Utils.remove_newline (Utils.remove_quotes (json_metricsOutput)))
      ~cg: (Utils.remove_newline (Utils.remove_quotes (json_cg)))
      ~cgRoots: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_cgRoots)
      ~cgServices: json_cgServices
      ~wpRte: json_wpRte
      ~wpCheckMemoryModel: json_wpCheckMemoryModel
      ~wpVolatile: json_wpVolatile
      ~wpGen: json_wpGen
      ~wpDump: json_wpDump
      ~wpStatus: json_wpStatus
      ~wpSmokeTests: json_wpSmokeTests
      ();


      Printf.printf "save_configs : global_params length : %d\n%!" (List.length (Json.list result))
  | x -> 
    Printf.printf "Requested unknown configuration(s), error : %s\n\t%s\n%!" (Json.save_string ~pretty:true x) (Printexc.get_backtrace ())
