#!/usr/bin/env ocaml

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;

#use "topfind";;
#require "str";;

let wordcount_file = ref "/Users/mike/Dropbox/Oxford/thesis/wordcount"

let read_wordcount () =
  let f = open_in !wordcount_file in
  let lines = ref [] in
  begin
    try while true do
          lines := input_line f :: !lines
        done
    with _ -> ()
  end;
  close_in f;
  List.rev !lines

let write_wordcount lines =
  let f = open_out !wordcount_file in
  let rec loop = function
  | [] -> ()
  | l::ls ->
     output_string f l;
     output_char f '\n';
     loop ls
  in
  loop lines;
  close_out f

let split_line l = Str.(split (regexp " ") l)

let date_of_line l = List.hd (split_line l)

let pairmap f (a,b) = (f a, f b)

let curry f (a,b) = f a b

let dates_equal a b = pairmap date_of_line (a,b) |> curry (=)

let rec dedup_lines acc cur = function
  | [] ->
     begin
       match cur with
       | None -> List.rev acc
       | Some c -> List.rev (c::acc)
     end
  | l::ls ->
     begin
       match cur with
       | None -> dedup_lines acc (Some l) ls
       | Some c ->
          if dates_equal l c
          then dedup_lines acc (Some l) ls
          else dedup_lines (c::acc) None ls
     end

let main : unit =
  if Array.length Sys.argv > 1
  then wordcount_file := Sys.argv.(1);

  read_wordcount ()
  |> dedup_lines [] None
  |> write_wordcount
