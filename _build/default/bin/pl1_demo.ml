open Printf

let () =
  if Array.length Sys.argv < 2 then begin
    eprintf "Usage: gastrolith-demo <file.pl1>\n";
    exit 1
  end;
  let file = Sys.argv.(1) in
  try
    let ic = open_in file in
    let src = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let prog = Pl1_core.Driver.parse_from_string src file in
    printf "Parsed %d statements\n" (List.length prog)
  with e ->
    eprintf "Error: %s\n" (Printexc.to_string e);
    exit 1