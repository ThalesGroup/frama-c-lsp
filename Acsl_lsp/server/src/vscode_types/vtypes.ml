module Configurations = struct
  type t = {
    framac_includePaths: string list ref;
    framac_sourceFiles: string list ref;
    framac_macros: string list ref;
  }

  let create ~framac_includePaths ~framac_sourceFiles ~framac_macros () = 
    { framac_includePaths; framac_sourceFiles; framac_macros }
end
