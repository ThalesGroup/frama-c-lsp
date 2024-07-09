(* Modules to hold vs code workspace configurations *)
module Configurations : sig
    type t = {
        framac_includePaths: string list ref;
        framac_sourceFiles: string list ref;
        framac_macros: string list ref;
      }
      val create : framac_includePaths: string list ref ->
                    framac_sourceFiles: string list ref->
                    framac_macros: string list ref-> unit -> t
end


