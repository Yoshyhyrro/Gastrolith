(* Configuration module interface *)

val debug_mode : bool ref
val output_ast : bool ref
val input_file : string ref
val output_file : string ref

val parse_args : unit -> unit
val print_usage : unit -> unit
