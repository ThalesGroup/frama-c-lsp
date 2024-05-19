module LSP_State = struct 
  (* type for client sessions *)
type session = {
  client_id: string;
  connected_at: float;
  last_active: float;
  status: string;
}

(* type for documents *)
type document = {
  uri: string;
  content: string;
  version: int;
  diagnostics: (int * string) list;
}

(* In-memory storage *)
let sessions : (string, session) Hashtbl.t = Hashtbl.create 10
let documents : (string, document) Hashtbl.t = Hashtbl.create 10

(* Example function to add a session *)
let add_session client_id connected_at =
  let new_session = {
    client_id;
    connected_at;
    last_active = connected_at;
    status = "active";
  } in
  Hashtbl.add sessions client_id new_session

end