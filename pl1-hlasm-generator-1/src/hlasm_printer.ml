(* HLASM Printer Module *)

let print_instruction instruction =
  match instruction with
  | "LOAD" -> "L"
  | "STORE" -> "ST"
  | "ADD" -> "A"
  | "SUB" -> "S"
  | "MULT" -> "M"
  | "DIV" -> "D"
  | "JUMP" -> "J"
  | "JUMPIF" -> "JI"
  | _ -> failwith "Unknown instruction"

let print_program program =
  List.iter (fun instr ->
    let output = print_instruction instr in
    print_endline output
  ) program

(* Example usage *)
let () =
  let sample_program = ["LOAD"; "ADD"; "STORE"; "JUMP"] in
  print_program sample_program