(* Minimal hand-written lexer replacement for ocamllex-generated module.
   from_channel reads all text from input and returns uppercase tokens split by whitespace.
*)

let uppercase s =
  String.map (fun c ->
    if 'a' <= c && c <= 'z' then Char.chr (Char.code c - 32) else c
  ) s

let from_channel ic =
  let buf = Buffer.create 1024 in
  (try
     while true do
       let line = input_line ic in
       Buffer.add_string buf line;
       Buffer.add_char buf ' '
     done
   with End_of_file -> ());
  let text = Buffer.contents buf in
  let ws = Str.split (Str.regexp "[ \t\r\n]+") text in
  List.map uppercase ws
