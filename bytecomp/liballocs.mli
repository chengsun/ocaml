open Lambda

module C : sig
  type toplevel
end

module Emitcode : sig
  val to_file: out_channel -> string -> string ->
    required_globals:Ident.Set.t -> C.toplevel list -> unit
          (* Arguments:
               channel on output file
               name of compilation unit implemented
               path of c file being written
               required_globals: list of compilation units that must be
                 evaluated before this one
               list of instructions to emit *)
end

val compile_implementation : string -> lambda -> C.toplevel list
