open Lambda

module C : sig
  type toplevel
end

module Emitcode : sig
  val to_file: out_channel -> string -> string -> C.toplevel list -> unit
          (* Arguments:
               channel on output file
               name of compilation unit implemented
               path of c file being written
               list of instructions to emit *)
end

val compile_implementation : string -> lambda -> C.toplevel list
