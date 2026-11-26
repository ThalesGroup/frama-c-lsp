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

(* caller/callee: files; caller_fn: calling function; symbol: called function *)
type edge = { caller : string; callee : string; caller_fn : string; symbol : string }

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
        Hashtbl.replace acc { caller; callee;
                              caller_fn = Kernel_function.get_name src;
                              symbol = Kernel_function.get_name dst } ()
    end) (Callgraph.Cg.get ());
  Hashtbl.fold (fun e () l -> e :: l) acc []


  let unresolved_calls resolved =
  let module G = Callgraph.Cg.G in
  let in_callgraph = Cil_datatype.Stmt.Hashtbl.create 64 in
  G.iter_edges_e (fun e -> Cil_datatype.Stmt.Hashtbl.replace in_callgraph (G.E.label e) ())
    (Callgraph.Cg.get ());
  let res = ref [] in
  let v = object
    inherit Visitor.frama_c_inplace
    method! vstmt_aux s =
      (match s.skind with
       | Instr (Call _)
         when not (Cil_datatype.Stmt.Hashtbl.mem in_callgraph s)
           && not (Cil_datatype.Stmt.Hashtbl.mem resolved s) ->
           let loc = Cil_datatype.Stmt.loc s in
           res := (file_of_loc loc, line_of_loc loc) :: !res
       | _ -> ());
      Cil.DoChildren
  end in
  Visitor.visitFramacFileSameGlobals v (Ast.get ());
  !res

let declared_edges () =
  let edges = ref [] in
  let resolved = Cil_datatype.Stmt.Hashtbl.create 64 in
  let v = object (self)
    inherit Visitor.frama_c_inplace
    method! vstmt_aux s =
      (match s.skind with
       | Instr (Call _) ->
           (match Dyncall.get s with
            | None -> ()
            | Some (_, kfs) ->
                Cil_datatype.Stmt.Hashtbl.replace resolved s ();
                let caller_kf = Option.get self#current_kf in
                let caller_fn = Kernel_function.get_name caller_kf in
                let caller = file_of_loc (Kernel_function.get_location caller_kf) in
                List.iter (fun callee_kf ->
                    let callee = file_of_loc (Kernel_function.get_location callee_kf) in
                    let symbol = Kernel_function.get_name callee_kf in
                    if caller <> callee then
                      edges := { caller; callee; caller_fn; symbol } :: !edges
                  ) kfs)
       | _ -> ());
      Cil.DoChildren
  end in
  Visitor.visitFramacFileSameGlobals v (Ast.get ());
  (!edges, resolved)



type exclusion = {
  x_from : string option; x_to : string option;
  x_caller : string option; x_callee : string option;
  x_from_caller : string option; x_from_callee : string option;
  x_reason : string;
}

let load_exclusions path =
  let json = Json.load_string (read_file path) in
  let field kv k = match List.assoc_opt k kv with Some (`String s) -> Some s | _ -> None in
  match json with
  | `Assoc top ->
      (match List.assoc_opt "exclusions" top with
       | Some (`List l) ->
           List.filter_map (function
             | `Assoc kv ->
                 Some { x_from = field kv "from"; x_to = field kv "to";
                        x_caller = field kv "caller"; x_callee = field kv "callee";
                        x_from_caller = field kv "fromCaller"; x_from_callee = field kv "fromCallee";
                        x_reason = Option.value ~default:"" (field kv "reason") }
             | _ -> None) l
       | _ -> [])
  | _ -> []

let kf_of_name name = try Some (Globals.Functions.find_by_name name) with Not_found -> None

let reachable g src dst =
  let module G = Callgraph.Cg.G in
  let seen = Kernel_function.Hashtbl.create 64 in
  let rec visit = function
    | [] -> false
    | kf :: rest ->
        if Kernel_function.equal kf dst then true
        else if Kernel_function.Hashtbl.mem seen kf || not (G.mem_vertex g kf) then visit rest
        else begin
          Kernel_function.Hashtbl.replace seen kf ();
          visit (G.succ g kf @ rest)
        end
  in
  visit [src]

let context_ok g x caller_fn =
  match x.x_from_caller, x.x_from_callee with
  | None, None -> true
  | Some c2, Some f2 ->
      (match kf_of_name c2, kf_of_name f2, kf_of_name caller_fn with
       | Some k2, Some kf2, Some k1 ->
           Callgraph.Cg.G.mem_vertex g k2 && Callgraph.Cg.G.mem_edge g k2 kf2 && reachable g kf2 k1
       | _ -> false)
  | _ -> false                                   (* only one of fromCaller / fromCallee *)

let callers_of g name =
  match kf_of_name name with
  | Some kf when Callgraph.Cg.G.mem_vertex g kf ->
      List.map Kernel_function.get_name (Callgraph.Cg.G.pred g kf)
  | _ -> []

let opt_matches o v = match o with None -> true | Some s -> s = v

let is_group_level x = x.x_from <> None && x.x_to <> None && x.x_caller = None && x.x_callee = None

let find_exclusion g exclusions gs gd e =
  let rec go i = function
    | [] -> None
    | x :: rest ->
        if is_group_level x then
          if opt_matches x.x_from gs && opt_matches x.x_to gd then Some (i, x, true) else go (i + 1) rest
        else if x.x_caller = Some e.caller_fn && x.x_callee = Some e.symbol
             && opt_matches x.x_from gs && opt_matches x.x_to gd then
          Some (i, x, context_ok g x e.caller_fn)
        else go (i + 1) rest
  in
  go 0 exclusions

let context_string x =
  match x.x_from_caller, x.x_from_callee with
  | Some c2, Some f2 -> c2 ^ " -> " ^ f2
  | _ -> ""

(* ---------- Aggregation + JSON ---------- *)

let compute_and_serialize id groups_file =
  let file_group = load_file_groups groups_file in
  let exclusions = load_exclusions groups_file in
  let g = Callgraph.Cg.get () in
  let cells = Hashtbl.create 16 and unassigned = Hashtbl.create 16 in
  let used = Hashtbl.create 16 in                         (* exclusion index -> () *)
  let excluded = ref [] and invalid = ref [] in
  let decl_edges, resolved = declared_edges () in
  let all_edges = file_edges () @ decl_edges in
  List.iter (fun e ->
    match group_of file_group e.caller, group_of file_group e.callee with
    | Some (gs_name, gs_kind), Some (gd_name, gd_kind) when gs_name <> gd_name ->
        let keep () =
          let l = Option.value ~default:[] (Hashtbl.find_opt cells (gs_name, gs_kind, gd_name, gd_kind)) in
          Hashtbl.replace cells (gs_name, gs_kind, gd_name, gd_kind) (e :: l) in
        (match find_exclusion g exclusions gs_name gd_name e with
         | Some (i, x, true) ->
             Hashtbl.replace used i ();
             excluded := (gs_name, gd_name, not (allowed gs_kind gd_kind), e, x) :: !excluded
         | Some (i, x, false) ->
             (* context not verified: the call stays in the matrix and is reported *)
             Hashtbl.replace used i ();
             invalid := (gs_name, gd_name, e, x) :: !invalid;
             keep ()
         | None -> keep ())
    | None, _ -> Hashtbl.replace unassigned e.caller ()
    | _, None -> Hashtbl.replace unassigned e.callee ()
    | _ -> ()) all_edges;

  let edge_json e =
    `Assoc [ "caller", `String e.caller; "callee", `String e.callee;
             "caller_fn", `String e.caller_fn; "symbol", `String e.symbol ] in
  let with_fields e extra = match edge_json e with `Assoc kv -> `Assoc (kv @ extra) | j -> j in
  let excluded_json (gs, gd, violation, e, x) =
    with_fields e [ "from", `String gs; "to", `String gd; "violation", `Bool violation;
                    "reason", `String x.x_reason; "context", `String (context_string x) ] in
  let invalid_json (gs, gd, e, x) =
    with_fields e [ "from", `String gs; "to", `String gd; "reason", `String x.x_reason;
                    "context", `String (context_string x);
                    "callers", `List (List.map (fun n -> `String n) (callers_of g e.caller_fn)) ] in
  let exclusion_json x =
    let opt k = function Some v -> [ k, `String v ] | None -> [] in
    `Assoc (opt "from" x.x_from @ opt "to" x.x_to @ opt "caller" x.x_caller @ opt "callee" x.x_callee
            @ opt "fromCaller" x.x_from_caller @ opt "fromCallee" x.x_from_callee
            @ [ "reason", `String x.x_reason ]) in
  let unused = List.filteri (fun i _ -> not (Hashtbl.mem used i)) exclusions in
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
        `Assoc [ "file", `String f; "line", `Int l ]) (unresolved_calls resolved));
    "excluded", `List (List.map excluded_json !excluded);
    "invalid_exclusions", `List (List.map invalid_json !invalid);
    "unused_exclusions", `List (List.map exclusion_json unused);
  ] in
  let msg = Lsp_types.ResponseMessage.create
      ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result () in
  Json.save_string (Lsp_types.ResponseMessage.json_of_t msg)