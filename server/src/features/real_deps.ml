open Cil_types

type kind = Core | Feature | Lib

let cwd = Sys.getcwd ()

let file_of_loc (loc : Cil_types.location) =
  let p = Filepath.to_string (fst loc).Filepath.pos_path in
  if Filename.is_relative p then Filename.concat cwd p else p

let line_of_loc (loc : Cil_types.location) =
  (fst loc).Filepath.pos_lnum

let kind_of_string = function
  | "core" -> Core | "feature" -> Feature | "lib" -> Lib
  | s -> failwith ("dep_groups: unknown kind " ^ s)

let read_file path = In_channel.with_open_text path In_channel.input_all

(* file -> (group name, kind), from the client-provided table *)
let load_file_groups path =
  let json = Json.load_string (read_file path) in
  let kinds = Hashtbl.create 16 in
  (match Json.field "kinds" json with
   | `Assoc kv -> List.iter (fun (name, k) ->
       Hashtbl.replace kinds name (kind_of_string (Json.string k))) kv
   | _ -> ());
  let file_group = Hashtbl.create 256 in
  (match Json.field "fileGroups" json with
   | `Assoc kv -> List.iter (fun (file, name) ->
       let name = Json.string name in
       match Hashtbl.find_opt kinds name with
       | Some k -> Hashtbl.replace file_group file (name, k)
       | None -> ()) kv
   | _ -> ());
  file_group

(* group of a file = direct lookup in the table (no pattern matching) *)
let group_of file_group file = Hashtbl.find_opt file_group file

let allowed src dst =
  match src, dst with
  | _, Lib | Feature, Core | Core, Core -> true
  | _ -> false

type edge = { caller : string; callee : string; symbol : string }

let file_edges () =
  let module G = Callgraph.Cg.G in
  let acc = Hashtbl.create 256 in
  G.iter_edges_e (fun e ->
    let src = G.E.src e and dst = G.E.dst e in
    if Kernel_function.is_definition src && Kernel_function.is_definition dst then begin
      let caller = file_of_loc (Kernel_function.get_location src) in
      (* Une fonction static est locale à son unité de compilation :
         on la rattache au fichier appelant plutôt qu'à son fichier de
         définition (qui peut être un header inclus). L'arête devient alors
         interne (caller = callee) et sera ignorée. *)
      let dst_vi = Kernel_function.get_vi dst in
      let callee =
        if dst_vi.vstorage = Static then caller
        else file_of_loc (Kernel_function.get_location dst)
      in
      if caller <> callee then
        Hashtbl.replace acc { caller; callee; symbol = Kernel_function.get_name dst } ()
    end) (Callgraph.Cg.get ());
  Hashtbl.fold (fun e () l -> e :: l) acc []
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
  let file_group = load_file_groups groups_file in
  let cells = Hashtbl.create 16 and unassigned = Hashtbl.create 16 in
  List.iter (fun e ->
    match group_of file_group e.caller, group_of file_group e.callee with
    | Some (gs_name, gs_kind), Some (gd_name, gd_kind) when gs_name <> gd_name ->
        let l = Option.value ~default:[] (Hashtbl.find_opt cells (gs_name, gs_kind, gd_name, gd_kind)) in
        Hashtbl.replace cells (gs_name, gs_kind, gd_name, gd_kind) (e :: l)
    | None, _ -> Hashtbl.replace unassigned e.caller ()
    | _, None -> Hashtbl.replace unassigned e.callee ()
    | _ -> ()) (file_edges ());

  let edge_json e =
    `Assoc [ "caller", `String e.caller; "callee", `String e.callee; "symbol", `String e.symbol ] in
  let cell_json (gs, _, gd, _) es =
    `Assoc [ "from", `String gs; "to", `String gd;
             "count", `Int (List.length es); "edges", `List (List.map edge_json es) ] in
  let matrix, violations =
    Hashtbl.fold (fun (gs, gsk, gd, gdk) es (m, v) ->
      let c = cell_json (gs, gsk, gd, gdk) es in
      if allowed gsk gdk then c :: m, v else m, c :: v) cells ([], []) in

  (* group names, deduplicated, for the header *)
  let names = Hashtbl.create 16 in
  Hashtbl.iter (fun _ (name, _) -> Hashtbl.replace names name ()) file_group;

  let result : Json.t = `Assoc [
    "groups", `List (Hashtbl.fold (fun n () l -> `String n :: l) names []);
    "matrix", `List matrix;
    "violations", `List violations;
    "unassigned", `List (Hashtbl.fold (fun f () l -> `String f :: l) unassigned []);
    "unresolved_calls", `List (List.map (fun (f, l) ->
        `Assoc [ "file", `String f; "line", `Int l ]) (unresolved_calls ()));
  ] in
  let msg = Lsp_types.ResponseMessage.create
      ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result () in
  Json.save_string (Lsp_types.ResponseMessage.json_of_t msg)