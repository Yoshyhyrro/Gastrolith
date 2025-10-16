(* preprocess.ml - card/Angstrom style preprocessor
   Implements 80-column PL/I card rules: strip columns beyond 72, handle continuation
*)

let trim_card_line line =
  let len = String.length line in
  if len > 0 && line.[0] = '*' then None else
  let code_part = if len >= 72 then String.sub line 0 72 else line in
  Some code_part

let is_continuation line =
  let len = String.length line in
  len >= 72 && (line.[71] = '+' || line.[71] = '-')

let preprocess_cards s =
  let lines = String.split_on_char '\n' s in
  let rec aux acc current = function
    | [] -> List.rev (if current <> "" then current :: acc else acc)
    | line :: rest ->
        match trim_card_line line with
        | None -> aux acc current rest
        | Some code ->
            if is_continuation code then
              let merged = if current = "" then String.trim code else current ^ " " ^ String.trim code in
              aux acc merged rest
            else if current = "" then
              aux acc (String.trim code) rest
            else
              aux (current :: acc) (String.trim code) rest
  in
  aux [] "" lines

let normalize_source s =
  preprocess_cards s |> String.concat "\n"
