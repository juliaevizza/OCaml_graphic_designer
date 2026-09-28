(** Command-line interface for the Picture This generator. *)
let () =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <input.pic> <output.svg>\n" Sys.argv.(0);
    exit 1);
  let input_filename = Sys.argv.(1) in
  let output_filename = Sys.argv.(2) in
  match A1.Generator.generate input_filename output_filename with
  | Ok () -> ()
  | Error message ->
      Printf.eprintf "%s\n" message;
      exit 1