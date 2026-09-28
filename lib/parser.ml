(** Parser for Picture This language. *)

open Picture
exception ParseError of string

(** [fail line message] will raise an error to the user at the [line] it is at. *)
let fail line message =
  raise (ParseError (Printf.sprintf "Line %d: %s" line message))

(** [words line] splits [line] on spaces and discards empty pieces. *)
let words line =
  line |> String.split_on_char ' ' |> List.filter (fun word -> word <> "")

(** [prepare lines] trims input, removes blank lines, and tokenizes the rest. *)
let prepare lines =
  lines
  |> List.filter_map (fun (line_number, line) ->
      let trimmed = String.trim line in
      if trimmed = "" then None else Some (line_number, words trimmed))

(** [color line text] interprets a named color or reports an error. *)
let color line = function
  | "black" -> Black
  | "white" -> White
  | "gray" -> Gray
  | "red" -> Red
  | "orange" -> Orange
  | "yellow" -> Yellow
  | "green" -> Green
  | "blue" -> Blue
  | "purple" -> Purple
  | "pink" -> Pink
  | "brown" -> Brown
  | "navy" -> Navy
  | "teal" -> Teal
  | "gold" -> Gold
  | "cream" -> Cream
  | unknown -> fail line (Printf.sprintf "unknown color %S" unknown)

(** [number line label text] parses a finite floating-point number. *)
let number line label text =
  match float_of_string_opt text with
  | Some value when Float.is_finite value -> value
  | _ -> fail line (Printf.sprintf "%s must be a finite number" label)

(** [positive line label text] parses a strictly positive finite number. *)
let positive line label text =
  let value = number line label text in
  if value > 0. then value
  else fail line (Printf.sprintf "%s must be positive" label)

(** [parse_repeats_count line text] parses a nonnegative integer repetition count. *)
let parse_repeat_count line text =
  match int_of_string_opt text with
  | Some value when value >= 0 -> value
  | _ -> fail line "repeat count must be a nonnegative integer"

(** [transformation line tokens] parses exactly one transformation. *)
let transformation line = function
  | [ "translate"; dx; dy ] ->
      Translate
        (number line "translation dx" dx, number line "translation dy" dy)
  | [ "rotate"; degrees ] -> Rotate (number line "rotation" degrees)
  | [ "scale"; factor ] -> Scale (positive line "scale factor" factor)
  | _ -> fail line "malformed transformation"

(** [missing_end line kind] reports a compound element with no closing [end]. *)
let missing_end line kind =
  fail line (Printf.sprintf "%s block is missing its matching end" kind)

(** [elements opener lines] parses elements through the current block's end.
    [opener] identifies an context block, if one exists. *)
let rec elements opener = function
  | [] -> (
      match opener with
      | None -> ([], [])
      | Some (line, kind) -> missing_end line kind)
  | (line, [ "end" ]) :: rest -> (
      match opener with
      | None -> fail line "unexpected end"
      | Some _ -> ([], rest))
  | (line, "transform" :: transform_words) :: rest ->
      let transform = transformation line transform_words in
      let body, after_body = elements (Some (line, "transform")) rest in
      let siblings, remaining = elements opener after_body in
      (Transform (transform, body) :: siblings, remaining)
  | (line, "repeat" :: count_text :: transform_words) :: rest ->
      let repetitions = parse_repeat_count line count_text in
      let transform = transformation line transform_words in
      let body, after_body = elements (Some (line, "repeat")) rest in
      let siblings, remaining = elements opener after_body in
      (Repeat (repetitions, transform, body) :: siblings, remaining)
  | (line, [ "circle"; center_x; center_y; radius; fill ]) :: rest ->
      let element =
        Circle
          {
            center_x = number line "circle center x" center_x;
            center_y = number line "circle center y" center_y;
            radius = positive line "circle radius" radius;
            fill = color line fill;
          }
      in
      let siblings, remaining = elements opener rest in
      (element :: siblings, remaining)
  | (line, [ "rectangle"; x; y; width; height; fill ]) :: rest ->
      let element =
        Rectangle
          {
            x = number line "rectangle x" x;
            y = number line "rectangle y" y;
            width = positive line "rectangle width" width;
            height = positive line "rectangle height" height;
            fill = color line fill;
          }
      in
      let siblings, remaining = elements opener rest in
      (element :: siblings, remaining)
  | (line, [ "line"; x1; y1; x2; y2; stroke; stroke_width ]) :: rest ->
      let element =
        Line
          {
            x1 = number line "line x1" x1;
            y1 = number line "line y1" y1;
            x2 = number line "line x2" x2;
            y2 = number line "line y2" y2;
            stroke = color line stroke;
            stroke_width = positive line "line width" stroke_width;
          }
      in
      let siblings, remaining = elements opener rest in
      (element :: siblings, remaining)
  | (line, "text" :: x :: y :: size :: fill :: contents) :: rest ->
      if contents = [] then fail line "text contents must not be empty";
      let element =
        Text
          {
            x = number line "text x" x;
            y = number line "text y" y;
            size = positive line "text size" size;
            fill = color line fill;
            contents = String.concat " " contents;
          }
      in
      let siblings, remaining = elements opener rest in
      (element :: siblings, remaining)
  | (line, command :: _) :: _ ->
      fail line (Printf.sprintf "unknown or malformed command %S" command)
  | (line, []) :: _ -> fail line "internal error: empty token list"

(** [parse_canvas lines] parses a complete prepared Picture This program. *)
let parse_canvas = function
  | [] -> raise (ParseError "Input contains no canvas declaration")
  | (line, [ "canvas"; width; height; background ]) :: rest ->
      let width = positive line "canvas width" width in
      let height = positive line "canvas height" height in
      let background =
        if background = "none" then None else Some (color line background)
      in
      let elements, remaining = elements None rest in
      if remaining <> [] then
        fail line "internal error: unconsumed input after picture";
      { width; height; background; elements }
  | (line, _) :: _ -> fail line "expected canvas width height background"

(** [parse lines] parses numbered source [lines], returning the first error. *)
let parse lines =
  try Ok (parse_canvas (prepare lines))
  with ParseError message -> Error message
