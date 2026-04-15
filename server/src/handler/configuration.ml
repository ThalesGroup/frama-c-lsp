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
          {"section": "kernel.serverPort"},
          {"section": "kernel.includePaths"},
          {"section": "kernel.sourceFiles"},
          {"section": "kernel.macros"},
          {"section": "kernel.macroStrategiesFunctionPrefix"},
          {"section": "kernel.sourceFileStrategies"},
          {"section": "kernel.sourceFileMetacsl"},
          {"section": "kernel.machdep"},
          {"section": "kernel.keepUnusedFunctions"},
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
          {"section": "uncast.active"},
          {"section": "uncast.lshiftAsMul"},
          {"section": "uncast.rshiftAsDiv"},
          {"section": "uncast.endianness"},
          {"section": "metacsl.active"},
          {"section": "metacsl.checks"},
          {"section": "metacsl.noSimpl"},
          {"section": "metacsl.noCheckExt"},
          {"section": "metacsl.numberAssertions"},
          {"section": "metacsl.checkCalleeAssigns"},
          {"section": "ccdoc.active"},
          {"section": "ccdoc.coverageVerif"},
          {"section": "ccdoc.latex"}
        ]
      }
    |}

  type t = {
    acslLsp : int;
    serverPort:int;
    includePaths : string list;
    sourceFiles : string list;
    macros : string list;
    macroStrategiesFunctionPrefix : string;
    sourceFileStrategies : string list;
    sourceFileMetacsl : string list;
    machdep : string;
    keepUnusedFunctions : string;
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
    uncastActive : bool;
    uncastLshiftAsMul : bool;
    uncastRshiftAsDiv : bool;
    uncastEndianness : string;
    metacslActive: bool;
    metacslChecks: bool;
    metacslNoSimpl: bool;
    metacslNoCheckExt: bool;
    metacslNumberAssertions: bool;
    metacslCheckCalleeAssigns: string list;
    ccdocActive : bool;
    ccdocCoverageVerif : bool;
    ccdocLatex : bool
  }

  let create
    ~acslLsp 
    ~serverPort
    ~includePaths
    ~sourceFiles
    ~macros
    ~macroStrategiesFunctionPrefix
    ~sourceFileStrategies
    ~sourceFileMetacsl
    ~machdep 
    ~keepUnusedFunctions
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
    ~ccdocActive
    ~ccdocCoverageVerif
    ~ccdocLatex
    ()
    =
    {
      acslLsp;
      serverPort;
      includePaths;
      sourceFiles;
      macros;
      macroStrategiesFunctionPrefix;
      sourceFileStrategies;
      sourceFileMetacsl;
      machdep;
      keepUnusedFunctions;
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
      uncastActive;
      uncastLshiftAsMul;
      uncastRshiftAsDiv;
      uncastEndianness;
      metacslActive;
      metacslChecks;
      metacslNoSimpl;
      metacslNoCheckExt;
      metacslNumberAssertions;
      metacslCheckCalleeAssigns;
      ccdocActive;
      ccdocCoverageVerif;
      ccdocLatex
    }

let global_params = ref
  (create 
  ~acslLsp:0
  ~serverPort:0
  ~includePaths:[]
  ~sourceFiles:[]
  ~macros:[]
  ~macroStrategiesFunctionPrefix:""
  ~sourceFileStrategies:[]
  ~sourceFileMetacsl: []
  ~machdep:""
  ~keepUnusedFunctions:"none"
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
  ~ccdocActive: false
  ~ccdocCoverageVerif: false
  ~ccdocLatex: false
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
        `Int json_serverPort;
        `List json_includePaths;
        `List json_sourceFiles;
        `List json_macros;
        `String json_macroStrategiesFunctionPrefix;
        `List json_sourceFileStrategies;
        `List json_sourceFileMetacsl;
        `String json_machdep;
        `String json_keepUnusedFunctions;
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
        `Bool json_uncastActive;
        `Bool json_uncastLshiftAsMul;
        `Bool json_uncastRshiftAsDiv;
        `String json_uncastEndianness;
        `Bool json_metacslActive;
        `Bool json_metacslChecks;
        `Bool json_metacslNoSimpl;
        `Bool json_metacslNoCheckExt;
        `Bool json_metacslNumberAssertions;
        `List json_metacslCheckCalleeAssigns;
        `Bool json_ccdocActive;
        `Bool json_ccdocCoverageVerif;
        `Bool json_ccdocLatex;
      ] 
    -> 
      global_params := create 
      ~acslLsp: json_acslLsp
      ~serverPort: json_serverPort
      ~includePaths: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_includePaths )
      ~sourceFiles: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFiles)
      ~macros: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_macros)
      ~macroStrategiesFunctionPrefix: (Utils.remove_newline (Utils.remove_quotes (json_macroStrategiesFunctionPrefix)))
      ~sourceFileStrategies: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFileStrategies)
      ~sourceFileMetacsl: (List.map (fun x -> (Utils.remove_newline (Utils.remove_quotes (Json.save_string x)))) json_sourceFileMetacsl)
      ~machdep: (Utils.remove_newline (Utils.remove_quotes (json_machdep)))
      ~keepUnusedFunctions: json_keepUnusedFunctions
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
      ~ccdocActive: json_ccdocActive
      ~ccdocCoverageVerif: json_ccdocCoverageVerif
      ~ccdocLatex: json_ccdocLatex
      ();
      Options.Self.Debug.set json_acslLsp; (* !Configuration.global_params.acslLsp *)
      Options.Self.debug ~level:1 "save_configs : global_params length : %d\n%!" (List.length (Json.list result))
  | x -> 
    Options.Self.error "Requested unknown configuration(s), error : %s\n\t%s\n%!" (Json.save_string ~pretty:true x) (Printexc.get_backtrace ())