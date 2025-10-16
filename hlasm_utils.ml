(* utils.ml - Common utility parsers and helpers *)
open Angstrom
open Hlasm_ast

(* =============================================================================
   WHITESPACE AND TOKEN HANDLING
   ============================================================================= *)

let ws = skip_while (function ' ' | '\t' -> true | _ -> false)
let ws1 = skip (function ' ' | '\t' -> true | _ -> false) *> ws

(* Token combinator - automatically handles surrounding whitespace *)
let token p = ws *> p <* ws

(* =============================================================================
   POSITION TRACKING
   ============================================================================= *)

let current_pos =
  pos >>| fun pos -> 
    { line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol }

(* Generic position wrapper - attaches position to any parsed value *)
let with_pos p =
  current_pos >>= fun pos ->
  p >>| fun x -> (x, pos)

(* =============================================================================
   NUMERIC HELPERS
   ============================================================================= *)

let opt_int =
  option None (
    take_while1 (function '0'..'9' -> true | _ -> false) >>| fun s -> 
    Some (int_of_string s)
  )

let parse_int =
  take_while1 (function '0'..'9' -> true | _ -> false) >>= fun s ->
  try return (int_of_string s)
  with _ -> fail "Invalid integer"

(* =============================================================================
   CHARACTER CLASS PREDICATES
   ============================================================================= *)

let is_digit = function '0'..'9' -> true | _ -> false
let is_hex_digit = function '0'..'9' | 'A'..'F' | 'a'..'f' -> true | _ -> false
let is_alpha_start = function
  | 'A'..'Z' | 'a'..'z' | '@' | '$' | '#' | '_' -> true
  | _ -> false
let is_alphanum = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '@' | '$' | '#' | '_' -> true
  | _ -> false

(* =============================================================================
   VALIDATION HELPERS
   ============================================================================= *)

let validate_length ~max name actual =
  if actual > max then
    fail (Printf.sprintf "%s too long (max %d characters)" name max)
  else
    return ()

let validate_range ~min ~max name value =
  if value >= min && value <= max then
    return value
  else
    fail (Printf.sprintf "%s out of range [%d..%d]: %d" name min max value)

(* =============================================================================
   COMMON PATTERNS
   ============================================================================= *)

(* Parse a keyword (case-insensitive) followed by required whitespace *)
let keyword kw = string_ci kw <* ws1

(* Parse a keyword with an optional following parser *)
let keyword_opt kw p = keyword kw *> option None (p >>| Option.some)

(* Parse a keyword with a required following parser *)
let keyword_req kw p = keyword kw *> p

(* Parse a comma-separated list with at least one element *)
let comma_sep1 p = sep_by1 (char ',') p

(* Parse a comma-separated list (possibly empty) *)
let comma_sep p = sep_by (char ',') p

(* =============================================================================
   END OF LINE HANDLING
   ============================================================================= *)

let eol = choice [ void (char '\n'); end_of_input ]

let skip_to_eol = take_while (fun c -> c <> '\n')
