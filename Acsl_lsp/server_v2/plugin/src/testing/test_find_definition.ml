(*let test_find_defintion () = 
      Settings.Self.debug ~level:3 "-- Tests : Find predicate definition --\n";
      Settings.Self.debug ~level:3 "------------ Empty JSON query --\n";
      Find_def.find_def (Jsonrpc.parse_request "{}");
      Settings.Self.debug ~level:3 "------------ Invalid JSON query --\n";
      Find_def.find_def (Jsonrpc.parse_request "{jsonrpc:2.0}");
      Settings.Self.debug ~level:3 "------------ Valid JSON query with valid method --\n";
      Find_def.find_def (Jsonrpc.parse_request "{jsonrpc:2.0,id:1,method:\"textDocument/definition\",params:{\"textDocument\":{uri: \"/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/tests/find_predicate_definition/standard_acsl_predicates.c\"},position:{line: 138, character: 59}}}")
    *)