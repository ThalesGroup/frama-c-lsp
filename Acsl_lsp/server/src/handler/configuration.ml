let sections = {| {"items": [
          {"section": "acslLsp"}, 
          {"section": "kernel.includePaths"},
          {"section": "kernel.sourceFiles"},
          {"section": "kernel.macros"},
          {"section": "kernel.machdep"},
          {"section": "kernel.removeUnusedSpecifiedFunctions"},
          {"section": "kernel.aggressiveMerging"},
          {"section": "kernel.generatedSpecCustom"},
          {"section": "metrics.output"},
          {"section": "callgraph.output"},
          {"section": "callgraph.roots"},
          {"section": "callgraph.services"},
          {"section": "wp.noPruning"},
          {"section": "wp.rte"},
          {"section": "wp.checkMemoryModel"},
          {"section": "wp.noVolatile"},
          {"section": "wp.prover"},
          {"section": "wp.timeout"},
          {"section": "wp.session"},
          {"section": "wp.script"},
          {"section": "diagnostics.wp"},
          {"section": "uncast.active"},
          {"section": "uncast.lshiftAsMul"},
          {"section": "uncast.rshiftAsDiv"},
          {"section": "uncast.endianness"},
          {"section": "metacsl.active"},
          {"section": "metacsl.checks"},
          {"section": "metacsl.noSimpl"},
          {"section": "metacsl.noCheckExt"},
          {"section": "metacsl.numberAssertions"},
          {"section": "metacsl.checkCalleeAssigns"}
        ]
      }
    |}

  type t = {
    acslLsp : int;
    includePaths : string list;
    sourceFiles : string list;
    macros : string list;
    machdep : string;
    removeUnusedSpecifiedFunctions : bool;
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
    wpScript : string;
    diagnosticsWp : bool;
    uncastActive : bool;
    uncastLshiftAsMul : bool;
    uncastRshiftAsDiv : bool;
    uncastEndianness : string;
    metacslActive: bool;
    metacslChecks: bool;
    metacslNoSimpl: bool;
    metacslNoCheckExt: bool;
    metacslNumberAssertions: bool;
    metacslCheckCalleeAssigns: string list
  }

  let create
    ~acslLsp 
    ~includePaths
    ~sourceFiles
    ~macros
    ~machdep 
    ~removeUnusedSpecifiedFunctions 
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
    ~wpScript
    ~diagnosticsWp
    ~uncastActive
    ~uncastLshiftAsMul
    ~uncastRshiftAsDiv
    ~uncastEndianness
    ~metacslActive
    ~metacslChecks
    ~metacslNoSimpl
    ~metacslNoCheckExt
    ~metacslNumberAssertions
    ~metacslCheckCalleeAssigns
    ()
    =
    {
      acslLsp;
      includePaths;
      sourceFiles;
      macros;
      machdep;
      removeUnusedSpecifiedFunctions;
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
      wpScript;
      diagnosticsWp;
      uncastActive;
      uncastLshiftAsMul;
      uncastRshiftAsDiv;
      uncastEndianness;
      metacslActive;
      metacslChecks;
      metacslNoSimpl;
      metacslNoCheckExt;
      metacslNumberAssertions;
      metacslCheckCalleeAssigns
    }

let global_params = ref
  (create 
  ~acslLsp:4
  ~includePaths:[]
  ~sourceFiles:[]
  ~macros:[]
  ~machdep:""
  ~removeUnusedSpecifiedFunctions:false
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
  ~wpScript: ""
  ~diagnosticsWp: false
  ~uncastActive: false
  ~uncastLshiftAsMul: true
  ~uncastRshiftAsDiv: true
  ~uncastEndianness: "little"
  ~metacslActive: false
  ~metacslChecks: true
  ~metacslNoSimpl: true
  ~metacslNoCheckExt: true
  ~metacslNumberAssertions: true
  ~metacslCheckCalleeAssigns: []
  ())

let request_configurations : Json.json = 
  let json_params = (Json.load_string sections) in
  let lsp_notification = (Lsp_types.RequestMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Str ("ask_configs")) ~method_:"workspace/configuration" ~params:json_params ()) in
  let json_notification = Lsp_types.RequestMessage.json_of_t lsp_notification in
  json_notification

let save_configs (result:  Json.json) = 
  (* note : result arguments must be in the same order as in the configuration request *)
  match result with
  | `List [
        `Int json_acslLsp;
        `List json_includePaths;
        `List json_sourceFiles;
        `List json_macros;
        `String json_machdep;
        `Bool json_removeUnusedSpecifiedFunctions;
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
        `String json_wpScript;
        `Bool json_diagnosticsWp;
        `Bool json_uncastActive;
        `Bool json_uncastLshiftAsMul;
        `Bool json_uncastRshiftAsDiv;
        `String json_uncastEndianness;
        `Bool json_metacslActive;
        `Bool json_metacslChecks;
        `Bool json_metacslNoSimpl;
        `Bool json_metacslNoCheckExt;
        `Bool json_metacslNumberAssertions;
        `List json_metacslCheckCalleeAssigns
      ] 
    -> 
      global_params := create 
      ~acslLsp: json_acslLsp
      ~includePaths: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_includePaths )
      ~sourceFiles: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFiles)
      ~macros: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_macros)
      ~machdep: (Utils.remove_newline (Utils.remove_quotes (json_machdep)))
      ~removeUnusedSpecifiedFunctions: json_removeUnusedSpecifiedFunctions
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
      ~wpScript: json_wpScript
      ~diagnosticsWp: json_diagnosticsWp
      ~uncastActive: json_uncastActive
      ~uncastLshiftAsMul: json_uncastLshiftAsMul
      ~uncastRshiftAsDiv: json_uncastRshiftAsDiv
      ~uncastEndianness: json_uncastEndianness
      ~metacslActive: json_metacslActive
      ~metacslChecks: json_metacslChecks
      ~metacslNoSimpl: json_metacslNoSimpl
      ~metacslNoCheckExt: json_metacslNoCheckExt
      ~metacslNumberAssertions: json_metacslNumberAssertions
      ~metacslCheckCalleeAssigns: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_metacslCheckCalleeAssigns)
      ();


      Lsp.Self.debug ~level:4 "save_configs : global_params length : %d\n%!" (List.length (Json.list result))
  | x -> 
    Lsp.Self.debug ~level:3 "Requested unknown configuration(s), error : %s\n\t%s\n%!" (Json.save_string ~pretty:true x) (Printexc.get_backtrace ())
