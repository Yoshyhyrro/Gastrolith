open Cmdliner
open Printf

(* Minimal analyzer PoC: uses the repository's parser to load a program and
   runs simplistic text/AST-based heuristics to emit warnings. This is a
   prototype intended to run on x86_64 Windows as an ocaml executable.
   Note: for production, integrate with AST types and proper CFG analysis.
*)

let read_file fname =
  try
    let ic = open_in fname in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic; s
  with e ->
    eprintf "File error: %s\n" (Printexc.to_string e); exit 1

(* Very small text heuristics: detect lines with ALLOCATE and FREE in same
   lexical block (naive: same file). For demonstration only. *)
let analyze_text src filename =
  let lines = String.split_on_char '\n' src in
  let alloc_lines = ref [] in
  let free_lines = ref [] in
  let based_lines = ref [] in
  let addr_assigns = ref [] in
  List.iteri (fun i l ->
    let u = String.uppercase_ascii l in
    if String.contains u '"' then () ; (* skip trivial: keep simple *)
    if String.contains u 'A' then ();
    if String.contains u 'A' then ();
    if String.trim u = "" then () else (
      if String.exists u (fun c -> c = 'A') then ();
      if String.index_opt u 'A' <> None then ()
    );
    if Str.string_match (Str.regexp "\\bALLOCATE\\b") u 0 then
      alloc_lines := (i+1)::!alloc_lines;
    if Str.string_match (Str.regexp "\\bFREE\\b") u 0 then
      free_lines := (i+1)::!free_lines;
    if Str.string_match (Str.regexp "\\bBASED\\b") u 0 then
      based_lines := (i+1)::!based_lines;
    if Str.string_match (Str.regexp "\\bADDR\\b") u 0 then
      addr_assigns := (i+1)::!addr_assigns
  ) lines;

  (* Emit simple warnings *)
  List.iter (fun ln ->
    printf "%s:%d: Warning: possible ALLOCATE without matching FREE in same
scope (heuristic).\n" filename ln
  ) (List.rev !alloc_lines);

  List.iter (fun ln ->
    printf "%s:%d: Info: FREE found (heuristic).\n" filename ln
  ) (List.rev !free_lines);

  (* Based without Addr check *)
  if !based_lines <> [] && !addr_assigns = [] then
    List.iter (fun ln ->
      printf "%s:%d: Warning: BASED usage without ADDR assignment detected
(heuristic).\n" filename ln
    ) (List.rev !based_lines)

let run file () =
  let src = read_file file in
  (* Try to parse using existing driver to ensure basic syntactic validity. *)
  let prog = try Pl1_core.Driver.parse_from_string src file with _ -> [] in
  printf "Parsed %d top-level statements (heuristic).\n" (List.length prog);
  analyze_text src file;
  `Ok ()

let file_arg =
  let doc = "PL/I source file (.pli or .pl1) to analyze." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "PL/I static analyzer (PoC)" in
  let info = Term.info "pl1_analyze" ~doc in
  Term.(ret (const run $ file_arg)), info

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> ()
