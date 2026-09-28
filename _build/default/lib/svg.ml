(** Recursive SVG renderer for typed Picture This pictures. *)

open Picture

(** [number value] formats a finite Picture This number for SVG. *)
let number value = Printf.sprintf "%.15g" value

(** [hex color] returns the specified SVG hexadecimal color. *)
let hex = function
  | Black -> "#111111"
  | White -> "#ffffff"
  | Gray -> "#808080"
  | Red -> "#c1121f"
  | Orange -> "#f77f00"
  | Yellow -> "#fcbf49"
  | Green -> "#2a9d8f"
  | Blue -> "#277da1"
  | Purple -> "#7b2cbf"
  | Pink -> "#e76f91"
  | Brown -> "#8d6e63"
  | Navy -> "#091226"
  | Teal -> "#008080"
  | Gold -> "#d9a441"
  | Cream -> "#fff1b0"

(** [reapply transform index] renders the 'cumulative transformation' for copy
    [index]. *)
let reapply transform index =
  let i = float_of_int index in
  match transform with
  | Translate (dx, dy) -> Printf.sprintf "translate(%s %s)" (number (i *. dx)) (number (i *. dy))
  | Rotate degrees -> Printf.sprintf "rotate(%s)" (number (i *. degrees))
  | Scale factor -> Printf.sprintf "scale(%s)" (number (factor ** i))

(** [once transform] renders a transformation applied exactly once. *)
let once = function
  | Translate (dx, dy) ->
      Printf.sprintf "translate(%s %s)" (number dx) (number dy)
  | Rotate degrees -> Printf.sprintf "rotate(%s)" (number degrees)
  | Scale factor -> Printf.sprintf "scale(%s)" (number factor)

(** [render_element element] recursively renders one SVG element. *)
let rec render_element = function
  | Circle { center_x; center_y; radius; fill } ->
      Printf.sprintf "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" fill=\"%s\" />\n"
        (number center_x) (number center_y) (number radius) (hex fill)
  | Rectangle { x; y; width; height; fill } ->
      Printf.sprintf
        "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"%s\" />\n"
        (number x) (number y) (number width) (number height) (hex fill)
  | Line { x1; y1; x2; y2; stroke; stroke_width } ->
      Printf.sprintf
        "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"%s\" \
         stroke-width=\"%s\" />\n"
        (number x1) (number y1) (number x2) (number y2) (hex stroke)
        (number stroke_width)
  | Text { x; y; size; fill; contents } ->
      Printf.sprintf
        "<text x=\"%s\" y=\"%s\" font-size=\"%s\" fill=\"%s\" \
         text-anchor=\"middle\">%s</text>\n"
        (number x) (number y) (number size) (hex fill) contents
  | Transform (transform, body) ->
      Printf.sprintf "<g transform=\"%s\">\n%s</g>\n" (once transform)
        (render_elements body)
  | Repeat (count, transform, body) ->
      List.init count (fun index ->
          Printf.sprintf "<g transform=\"%s\">\n%s</g>\n"
            (reapply transform index)
            (render_elements body))
      |> String.concat ""

(** [render_elements elements] renders elements in source order. *)
and render_elements elements =
  elements |> List.map render_element |> String.concat ""

(** [render picture] renders a complete SVG document. *)
let render picture =
  let width = number picture.width in
  let height = number picture.height in
  let background =
    match picture.background with
    | None -> ""
    | Some fill ->
        Printf.sprintf
          "<rect x=\"0\" y=\"0\" width=\"%s\" height=\"%s\" fill=\"%s\" />\n"
          width height (hex fill)
  in
  Printf.sprintf
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\" \
     viewBox=\"0 0 %s %s\">\n\
     %s%s</svg>\n"
    width height width height background
    (render_elements picture.elements)
