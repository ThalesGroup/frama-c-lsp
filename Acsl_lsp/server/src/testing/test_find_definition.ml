let run_test_find_defintion () = 
      Printf.printf "-- Tests : Find predicate definition --\n";
      Printf.printf "------------ Empty JSON query --\n";
      Find_def.find_def (Jsonrpc.parse_request "{}");
      Printf.printf "------------ Invalid JSON query --\n";
      Find_def.find_def (Jsonrpc.parse_request "{jsonrpc:2.0}");
      Printf.printf "------------ Valid JSON query with valid method --\n";
      Find_def.find_def (Jsonrpc.parse_request "{jsonrpc:2.0,id:1,method:\"textDocument/definition\",params:{\"textDocument\":{uri: \"/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/tests/find_predicate_definition/standard_acsl_predicates.c\"},position:{line: 138, character: 59}}}")
    