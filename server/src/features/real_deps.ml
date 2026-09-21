open Cil_types

(* ---------- Groups ---------- *)

type kind = Core | Feature | Lib
type group = { name : string; kind : kind; patterns : string list }

let cwd = Sys.getcwd ()

(* Frama-C may report a path relative to its cwd; make it absolute so it can
   be compared against the (absolute) group patterns. *)
let file_of_loc (loc : Cil_types.location) =
  let p = Filepath.to_string (fst loc).Filepath.pos_path in
  if Filename.is_relative p then Filename.concat cwd p else p

let line_of_loc (loc : Cil_types.location) =
  (fst loc).Filepath.pos_lnum

let kind_of_string = function
  | "core" -> Core | "feature" -> Feature | "lib" -> Lib
  | s -> failwith ("dep_groups: unknown kind " ^ s)

(* ---------- Path matching with wildcards ----------
   '*' matches any characters EXCEPT '/' (a single directory level),
   like a shell glob: dir/start*.c matches dir/start1.c but not dir/sub/x.c.
   A pattern ending with '/' (a directory) matches its whole subtree.
   Matching is done on the path SUFFIX, so it works whether the file path
   is absolute or relative. *)
let regex_of_pattern p =
  let buf = Buffer.create (String.length p * 2) in
  String.iter (fun c ->
    match c with
    | '*' -> Buffer.add_string buf "[^/]*"
    | '.' | '+' | '(' | ')' | '[' | ']' | '\\' | '$' | '^' | '|' | '?' | '{' | '}' ->
        Buffer.add_char buf '\\'; Buffer.add_char buf c
    | c -> Buffer.add_char buf c) p;
  if String.length p > 0 && p.[String.length p - 1] = '/'
  then Buffer.add_string buf ".*";
  Buffer.add_char buf '$';           (* anchored at the end only *)
  Str.regexp (Buffer.contents buf)

let matches pattern file =
  try ignore (Str.search_forward (regex_of_pattern pattern) file 0); true
  with Not_found -> false

let read_file path =
  In_channel.with_open_text path In_channel.input_all

let load_groups path =
  let json = Json.load_string (read_file path) in
  List.map (fun g ->
    { name = Json.string (Json.field "name" g);
      kind = kind_of_string (Json.string (Json.field "kind" g));
      patterns = List.map Json.string (Json.list (Json.field "paths" g)) })
    (Json.list (Json.field "groups" json))

(* First group whose one of its patterns matches the file, if any. *)
let group_of groups file =
  List.find_opt (fun g ->
    List.exists (fun p -> matches p file) g.patterns) groups

(* Architecture rule: anyone -> lib, feature -> core, core -> core. *)
let allowed src dst =
  match src.kind, dst.kind with
  | _, Lib | Feature, Core | Core, Core -> true
  | _ -> false

(* ---------- File -> file edges, from the callgraph ---------- *)

type edge = { caller : string; callee : string; symbol : string }

let file_edges () =
  let module G = Callgraph.Cg.G in
  let acc = Hashtbl.create 256 in
  G.iter_edges_e (fun e ->
    let src = G.E.src e and dst = G.E.dst e in
    (* No definition = libc / external function: ignored. *)
    if Kernel_function.is_definition src && Kernel_function.is_definition dst then begin
      let caller = file_of_loc (Kernel_function.get_location src) in
      let callee = file_of_loc (Kernel_function.get_location dst) in
      if caller <> callee then
        Hashtbl.replace acc { caller; callee; symbol = Kernel_function.get_name dst } ()
    end) (Callgraph.Cg.get ());
  Hashtbl.fold (fun e () l -> e :: l) acc []

(* Calls the callgraph did not resolve (function pointer without a `calls` clause). *)
let unresolved_calls () =
  let module G = Callgraph.Cg.G in
  let resolved = Cil_datatype.Stmt.Hashtbl.create 64 in
  G.iter_edges_e (fun e -> Cil_datatype.Stmt.Hashtbl.replace resolved (G.E.label e) ())
    (Callgraph.Cg.get ());
  let res = ref [] in
  let v = object
    inherit Visitor.frama_c_inplace
    method! vstmt_aux s =
      (match s.skind with
       | Instr (Call _) when not (Cil_datatype.Stmt.Hashtbl.mem resolved s) ->
           let loc = Cil_datatype.Stmt.loc s in
           res := (file_of_loc loc, line_of_loc loc) :: !res
       | _ -> ());
      DoChildren
  end in
  Visitor.visitFramacFileSameGlobals v (Ast.get ());
  !res

(* ---------- Aggregation + JSON ---------- *)

let compute_and_serialize id groups_file =
  let groups = load_groups groups_file in
  let cells = Hashtbl.create 16 and unassigned = Hashtbl.create 16 in
  List.iter (fun e ->
    match group_of groups e.caller, group_of groups e.callee with
    | Some gs, Some gd when gs.name <> gd.name ->
        let l = Option.value ~default:[] (Hashtbl.find_opt cells (gs, gd)) in
        Hashtbl.replace cells (gs, gd) (e :: l)
    | None, _ -> Hashtbl.replace unassigned e.caller ()
    | _, None -> Hashtbl.replace unassigned e.callee ()
    | _ -> ()) (file_edges ());

  let edge_json e =
    `Assoc [ "caller", `String e.caller; "callee", `String e.callee; "symbol", `String e.symbol ] in
  let cell_json (gs, gd) es =
    `Assoc [ "from", `String gs.name; "to", `String gd.name;
             "count", `Int (List.length es); "edges", `List (List.map edge_json es) ] in
  let matrix, violations =
    Hashtbl.fold (fun (gs, gd) es (m, v) ->
      let c = cell_json (gs, gd) es in
      if allowed gs gd then c :: m, v else m, c :: v) cells ([], []) in

  let result : Json.t = `Assoc [
    "groups", `List (List.map (fun g -> `String g.name) groups);
    "matrix", `List matrix;
    "violations", `List violations;
    "unassigned", `List (Hashtbl.fold (fun f () l -> `String f :: l) unassigned []);
    "unresolved_calls", `List (List.map (fun (f, l) ->
        `Assoc [ "file", `String f; "line", `Int l ]) (unresolved_calls ()));
  ] in
  let msg = Lsp_types.ResponseMessage.create
      ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result () in
  Json.save_string (Lsp_types.ResponseMessage.json_of_t msg)