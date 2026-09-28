(** Up top organization for the Picture This generator. *)

(** [read_numbered_lines input_filename] reads input and maps to tuples. 
    source line numbers. *)
let read_numbered_lines input_filename : ((int * string) list, string) result =
  try
    let lines = In_channel.with_open_text input_filename In_channel.input_lines in
    let numbered_lines = List.mapi (fun index line -> (index + 1, line)) lines in
    Ok numbered_lines
  with Sys_error message ->
    Error
      (Printf.sprintf "Unable to read input file %S: %s" input_filename message)

(** [write_svg output_filename svg] writes [svg] or returns an informed error. *)
let write_svg output_filename svg =
  try
    Out_channel.with_open_text output_filename (fun channel ->
        output_string channel svg);
    Ok ()
  with Sys_error message ->
    Error
      (Printf.sprintf "Unable to write output file %S: %s" output_filename
         message)

(** [generate input_filename output_filename] parses [input_filename] completely
    before creating or replacing [output_filename]. *)
let generate input_filename output_filename =
  match read_numbered_lines input_filename with
  | Error _ as error -> error
  | Ok lines -> (
      match Parser.parse lines with
      | Error _ as error -> error
      | Ok picture -> write_svg output_filename (Svg.render picture))
