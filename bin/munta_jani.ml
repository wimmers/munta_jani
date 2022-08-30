open Core
open Ppx_yojson_conv_lib
open Symta
open Munta

let pp_var_state = Util.pp_comma_list
  (fun ppf (x, i) ->
    Caml.Format.fprintf ppf "%s: %d" x (MC.to_int i)
  )

let pp_loc_state = Util.pp_comma_list Format.pp_print_text

let pp_state ppf (l, s) =
  Format.fprintf ppf "@[⟨@[%a@]@ |@ @[%a@]⟩@]" pp_loc_state l pp_var_state s

let jani_of_json_file s = JANI.model_of_yojson (Yojson.Safe.from_file s)

let do_it file_name property_name =
  try
    let jani = jani_of_json_file file_name in
    let model = Jani_to_munta.model jani in
    let property =
      match property_name with
      | None -> begin
        match List.hd jani.properties with
        | None -> failwith "No properties specified!"
        | Some p ->
          printf "No property given. Choosing: %s@." (p.name);
          p
        end
      | Some name ->
        let p = List.find jani.properties ~f:(fun p -> String.equal p.name name)
        in
        match p with
        | None -> failwith (sprintf "No property with name '%s'" name)
        | Some p -> p
    in
      print_endline "This JANI model was read:";
      print_endline (JANI.jani_to_json_string jani);
      print_endline "=========================================";
      let formula = Jani_to_munta.property_expression property.expression in
      let all_vars = MC.clks model @ MC.vars model in
      let l_0 = MC.l_0 model in
      let s_0 = MC.s_0 model in
      let () = List.iter all_vars ~f:Stdio.print_endline in
      print_endline "The JANI model in Munta's internal JANI representation:";
      let () = MC.show_model model |> print_endline in
      print_endline "=========================================";
      let () =
        Format.printf "@[Converted initial state:@ %a@]@." pp_state (l_0, s_0)
      in
      print_endline "=========================================";
      let () = Format.printf "@[Property:@ %s@]@." (MC.show_formula formula) in
      print_endline "=========================================";
      match MC.do_preproc_mc_jani1 model formula () with
      | MC.Result r ->
        print_endline "=========================================";
        print_endline "Result:";
        print_endline r
      | MC.Error es ->
        print_endline "=========================================";
        print_endline "Error:";
        List.iter ~f:print_endline es
  with Yojson_conv.Of_yojson_error (e, yojson) ->
    print_endline (Exn.to_string e);
    print_endline (Yojson.Safe.to_string yojson)

let command =
  Command.basic
    ~summary:"Check JANI model with Munta."
    Command.Let_syntax.(
      let%map_open file_name = anon ("filename" %: Filename_unix.arg_type)
      and property_name = anon (maybe ("formula" %: string))
      in
      fun () ->
        do_it file_name property_name
    )

let () = Command_unix.run command