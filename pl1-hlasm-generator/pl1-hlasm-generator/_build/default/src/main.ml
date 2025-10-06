open Printf

let () =
  printf "pl1-hlasm-generator: read PL/I from stdin, write HLASM to stdout\n";
  Generator.generate_from_channel stdin stdout;
  printf "done.\n";
  ()
