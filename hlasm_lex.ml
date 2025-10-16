(* lex.ml - Lexical analysis primitives *)
open Angstrom
open Utils

(* =============================================================================
   STRING LITERALS
   ============================================================================= *)

let quoted_string quote_char =
  char quote_char *> 
  take_while (fun c -> c <> quote_char && c <> '\n') <*
  char quote_char

let apostrophe_string = quoted_string '\''

(* =============================================================================
   COMMENTS
   ============================================================================= *)

let comment_line = char '*' *> take_while (fun c -> c <> '\n')

let full_line_comment = ws *> comment_line

(* =============================================================================
   DBCS (Double-Byte Character Set) SUPPORT
   ============================================================================= *)

(* Shift-Out / Shift-In control characters *)
let shift_out = char '\x0E'
let shift_in = char '\x0F'

(* Valid DBCS byte range *)
let is_dbcs_byte = function '\x41'..'\xFE' -> true | _ -> false
let dbcs_byte = satisfy is_dbcs_byte

(* Parse a single DBCS character (2 bytes between SO/SI) *)
let dbcs_char =
  shift_out *>
  lift2 (fun b1 b2 -> String.make 1 b1 ^ String.make 1 b2) 
    dbcs_byte dbcs_byte
  <* shift_in

(* Parse a string of DBCS characters *)
let dbcs_string = many1 dbcs_char >>| String.concat ""

(* =============================================================================
   IDENTIFIER COMPONENTS
   ============================================================================= *)

(* Parse an identifier name with length validation *)
let identifier_name ~max_length =
  peek_char >>= function
  | Some c when is_alpha_start c ->
      take_while is_alphanum >>= fun name ->
      if String.length name = 0 then
        fail "Empty identifier"
      else if String.length name > max_length then
        fail (Printf.sprintf "Identifier too long (max %d characters)" max_length)
      else
        return name
  | _ -> fail "Expected identifier"

(* Standard HLASM symbol name (max 63 chars) *)
let symbol_name = identifier_name ~max_length:63

(* Mnemonic (alphanumeric, case-insensitive) *)
let mnemonic =
  take_while1 (function 'A'..'Z' | 'a'..'z' | '0'..'9' -> true | _ -> false) >>|
  String.uppercase_ascii

(* Type code (alphabetic, case-insensitive) *)
let type_code =
  take_while1 (function 'A'..'Z' | 'a'..'z' -> true | _ -> false) >>|
  String.uppercase_ascii

(* =============================================================================
   NUMERIC LITERALS
   ============================================================================= *)

let decimal_digits = take_while1 is_digit

let hex_digits = take_while1 is_hex_digit

let binary_digits = take_while1 (function '0' | '1' -> true | _ -> false)

(* =============================================================================
   MIXED SBCS/DBCS CHARACTER PARSING
   ============================================================================= *)

(* Parse a single character that can be either SBCS or DBCS *)
let mixed_char =
  choice [
    dbcs_char;
    (satisfy (fun c -> c <> '\'' && c <> '\n') >>| String.make 1);
  ]

(* Parse a string that can contain mixed SBCS and DBCS characters *)
let mixed_string = many mixed_char >>| String.concat ""
