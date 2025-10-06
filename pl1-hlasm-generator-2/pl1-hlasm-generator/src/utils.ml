(* This file contains utility functions used throughout the project. *)

let rec split_string str delim =
  let rec aux acc start =
    try
      let idx = String.index_from str start delim in
      let part = String.sub str start (idx - start) in
      aux (part :: acc) (idx + 1)
    with Not_found ->
      let part = String.sub str start (String.length str - start) in
      List.rev (part :: acc)
  in
  aux [] 0

let trim_string str =
  let len = String.length str in
  let rec ltrim i =
    if i < len && String.get str i = ' ' then ltrim (i + 1) else i
  in
  let rec rtrim i =
    if i > 0 && String.get str (i - 1) = ' ' then rtrim (i - 1) else i
  in
  let start = ltrim 0 in
  let stop = rtrim len in
  String.sub str start (stop - start)

let read_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []