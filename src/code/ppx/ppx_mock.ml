open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let module_expr mapper module_expr =
  match module_expr.pmod_desc with
  | Pmod_extension (loc,payload) ->
     Printf.printf "[extension] %s\n" loc.txt;
     begin match payload with
           | PStr s -> failwith "TODO"
           | _ -> default_mapper.module_expr mapper module_expr
     end
  | _ -> default_mapper.module_expr mapper module_expr

let values = ref []

let record_sig_val v = values := v :: !values

let print_sig_item si = match si.psig_desc with
  | Psig_value v ->
     record_sig_val v;
     Printf.printf "recorded %s\n" v.pval_name.txt
  | _ -> print_endline "sig item"

let print_sig_types = function
  | Pmty_signature sis ->
     Printf.printf "Found %d sig items\n" (List.length sis);
     List.iter print_sig_item sis
  | _ -> ()

let attr_exists attr list =
  list
  |> List.map (function ({txt}, _) -> txt = attr)
  |> List.fold_left (&&) true

let module_type mapper module_type =
  if attr_exists "mock" module_type.pmty_attributes
  then print_sig_types module_type.pmty_desc;
  default_mapper.module_type mapper module_type

let () = Ast_mapper.register
           "ppx_mock"
           (fun argv ->
            { default_mapper with
              module_expr;
              module_type;
            } )
