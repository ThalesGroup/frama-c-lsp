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
 
let sections = {| {"items": [
          {"section": "kernel.lspDebug"}, 
          {"section": "kernel.includePaths"},
          {"section": "kernel.sourceFiles"},
          {"section": "kernel.macros"},
          {"section": "kernel.macroStrategiesFunctionPrefix"},
          {"section": "kernel.sourceFileStrategies"},
          {"section": "kernel.sourceFileMetacsl"},
          {"section": "kernel.machdep"},
          {"section": "kernel.removeUnusedSpecifiedFunctions"},
          {"section": "kernel.aggressiveMerging"},
          {"section": "kernel.generatedSpecCustom"},
          {"section": "kernel.inlineCalls"},
          {"section": "kernel.removeInlined"},
          {"section": "kernel.noAnnot"},
          {"section": "metrics.byFunction"},
          {"section": "callgraph.roots"},
          {"section": "callgraph.services"},
          {"section": "wp.noPruning"},
          {"section": "wp.rte"},
          {"section": "wp.checkMemoryModel"},
          {"section": "wp.noVolatile"},
          {"section": "wp.prover"},
          {"section": "wp.timeout"},
          {"section": "wp.par"},
          {"section": "wp.session"},
          {"section": "wp.script"},
          {"section": "wp.cache"},
          {"section": "wp.autoDepth"},
          {"section": "wp.autoWidth"},
          {"section": "wp.autoBacktrack"},
          {"section": "wp.filenameTruncation"},
          {"section": "diagnostics.wp"},
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
    macroStrategiesFunctionPrefix : string;
    sourceFileStrategies : string list;
    sourceFileMetacsl : string list;
    machdep : string;
    removeUnusedSpecifiedFunctions : bool;
    aggressiveMerging : bool;
    generatedSpecCustom : string list;
    inlineCalls: string list;
    removeInlined: string list;
    noAnnot: bool;
    metricsByFunction : bool;
    cgRoots : string list;
    cgServices : bool;
    wpPruning : bool;
    wpRte : bool;
    wpCheckMemoryModel : bool;
    wpVolatile : bool;
    wpProver : string;
    wpTimeout : int;
    wpPar : int;
    wpSession : string;
    wpScript : string;
    wpCache : string;
    wpAutoDepth : int;
    wpAutoWidth : int;
    wpAutoBacktrack : int;
    wpFilenameTruncation: int;
    diagnosticsWp : bool;
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
    ~macroStrategiesFunctionPrefix
    ~sourceFileStrategies
    ~sourceFileMetacsl
    ~machdep 
    ~removeUnusedSpecifiedFunctions 
    ~aggressiveMerging 
    ~generatedSpecCustom
    ~inlineCalls
    ~removeInlined
    ~noAnnot
    ~metricsByFunction
    ~cgRoots
    ~cgServices 
    ~wpPruning 
    ~wpRte 
    ~wpCheckMemoryModel 
    ~wpVolatile 
    ~wpProver
    ~wpTimeout
    ~wpPar
    ~wpSession
    ~wpScript
    ~wpCache
    ~wpAutoDepth
    ~wpAutoWidth
    ~wpAutoBacktrack
    ~wpFilenameTruncation
    ~diagnosticsWp
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
      macroStrategiesFunctionPrefix;
      sourceFileStrategies;
      sourceFileMetacsl;
      machdep;
      removeUnusedSpecifiedFunctions;
      aggressiveMerging;
      generatedSpecCustom;
      inlineCalls;
      removeInlined;
      noAnnot;
      metricsByFunction;
      cgRoots;
      cgServices;
      wpPruning;
      wpRte;
      wpCheckMemoryModel;
      wpVolatile;
      wpProver;
      wpTimeout;
      wpPar;
      wpSession;
      wpScript;
      wpCache;
      wpAutoDepth;
      wpAutoWidth;
      wpAutoBacktrack;
      wpFilenameTruncation;
      diagnosticsWp;
      metacslActive;
      metacslChecks;
      metacslNoSimpl;
      metacslNoCheckExt;
      metacslNumberAssertions;
      metacslCheckCalleeAssigns;
    }

let global_params = ref
  (create 
  ~acslLsp:0
  ~includePaths:[]
  ~sourceFiles:[]
  ~macros:[]
  ~macroStrategiesFunctionPrefix:""
  ~sourceFileStrategies:[]
  ~sourceFileMetacsl: []
  ~machdep:""
  ~removeUnusedSpecifiedFunctions:false
  ~aggressiveMerging:false
  ~generatedSpecCustom:[]
  ~inlineCalls:[]
  ~removeInlined:[]
  ~noAnnot:false
  ~metricsByFunction:false
  ~cgRoots:[]
  ~cgServices:false
  ~wpPruning:false
  ~wpRte:false
  ~wpCheckMemoryModel:false
  ~wpVolatile:false
  ~wpProver: "script,alt-ergo"
  ~wpTimeout: 2
  ~wpPar: 4
  ~wpSession: ""
  ~wpScript: ""
  ~wpCache: ""
  ~wpAutoDepth: 20
  ~wpAutoWidth: 1
  ~wpAutoBacktrack: 1
  ~wpFilenameTruncation: 220
  ~diagnosticsWp: false
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
        `String json_macroStrategiesFunctionPrefix;
        `List json_sourceFileStrategies;
        `List json_sourceFileMetacsl;
        `String json_machdep;
        `Bool json_removeUnusedSpecifiedFunctions;
        `Bool json_aggressiveMerging;
        `List json_generatedSpecCustom;
        `List json_inlineCalls;
        `List json_removeInlined;
        `Bool json_noAnnot;
        `Bool json_metricsByFunction;
        `List json_cgRoots;
        `Bool json_cgServices;
        `Bool json_wpPruning;
        `Bool json_wpRte;
        `Bool json_wpCheckMemoryModel;
        `Bool json_wpVolatile;
        `String json_wpProver;
        `Int json_wpTimeout;
        `Int json_wpPar;
        `String json_wpSession;
        `String json_wpScript;
        `String json_wpCache;
        `Int json_wpAutoDepth;
        `Int json_wpAutoWidth;
        `Int json_wpAutoBacktrack;
        `Int json_wpFilenameTruncation;
        `Bool json_diagnosticsWp;
        `Bool json_metacslActive;
        `Bool json_metacslChecks;
        `Bool json_metacslNoSimpl;
        `Bool json_metacslNoCheckExt;
        `Bool json_metacslNumberAssertions;
        `List json_metacslCheckCalleeAssigns;
      ] 
    -> 
      global_params := create 
      ~acslLsp: json_acslLsp
      ~includePaths: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_includePaths )
      ~sourceFiles: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFiles)
      ~macros: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_macros)
      ~macroStrategiesFunctionPrefix: (Utils.remove_newline (Utils.remove_quotes (json_macroStrategiesFunctionPrefix)))
      ~sourceFileStrategies: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFileStrategies)
      ~sourceFileMetacsl: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFileMetacsl)
      ~machdep: (Utils.remove_newline (Utils.remove_quotes (json_machdep)))
      ~removeUnusedSpecifiedFunctions: json_removeUnusedSpecifiedFunctions
      ~aggressiveMerging: json_aggressiveMerging
      ~generatedSpecCustom: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_generatedSpecCustom)
      ~inlineCalls: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_inlineCalls)
      ~removeInlined: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_removeInlined)
      ~noAnnot: json_noAnnot
      ~metricsByFunction: json_metricsByFunction
      ~cgRoots: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_cgRoots)
      ~cgServices: json_cgServices
      ~wpPruning: json_wpPruning
      ~wpRte: json_wpRte
      ~wpCheckMemoryModel: json_wpCheckMemoryModel
      ~wpVolatile: json_wpVolatile
      ~wpProver: json_wpProver
      ~wpTimeout: json_wpTimeout
      ~wpPar: json_wpPar
      ~wpSession: json_wpSession
      ~wpScript: json_wpScript
      ~wpCache: json_wpCache
      ~wpAutoDepth: json_wpAutoDepth
      ~wpAutoWidth: json_wpAutoWidth
      ~wpAutoBacktrack: json_wpAutoBacktrack
      ~wpFilenameTruncation: json_wpFilenameTruncation
      ~diagnosticsWp: json_diagnosticsWp
      ~metacslActive: json_metacslActive
      ~metacslChecks: json_metacslChecks
      ~metacslNoSimpl: json_metacslNoSimpl
      ~metacslNoCheckExt: json_metacslNoCheckExt
      ~metacslNumberAssertions: json_metacslNumberAssertions
      ~metacslCheckCalleeAssigns: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_metacslCheckCalleeAssigns)
      ();
      Options.Self.Debug.set json_acslLsp; (* !Configuration.global_params.acslLsp *)
      Options.Self.debug ~level:1 "save_configs : global_params length : %d\n%!" (List.length (Json.list result))
  | x -> 
    Options.Self.error "Requested unknown configuration(s), error : %s\n\t%s\n%!" (Json.save_string ~pretty:true x) (Printexc.get_backtrace ())
