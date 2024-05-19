module type B = sig
  type t
  val t_of_json : Json.t -> t
  val json_of_t : t -> Json.t
end