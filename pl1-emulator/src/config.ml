(* Configuration module implementation *)

let debug_mode = ref false
let output_ast = ref false
let input_file = ref ""
let output_file = ref ""

let usage_msg = "pl1-parser [options] <input_file>"

let spec_list = [
  ("-debug", Arg.Set debug_mode, " Enable debug output");
  ("-ast", Arg.Set output_ast, " Output AST representation");
  ("-o", Arg.Set_string output_file, " Set output file");
]

let set_input_file filename =
  if !input_file = "" then
    input_file := filename
  else
    failwith "Multiple input files specified"

let parse_args () =
  Arg.parse spec_list set_input_file usage_msg;
  if !input_file = "" then (
    print_usage ();
    exit 1
  )

let print_usage () =
  Arg.usage spec_list usage_msg
