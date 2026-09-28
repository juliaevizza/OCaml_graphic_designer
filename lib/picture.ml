(** Typed data representation of Picture This pictures. Breaks each
potential element (adjective) down into a discrete data struct we can
operate on predictably. Any additional "objects" should go into here 
with a a corresponding position in parse.*)

(** A color supported by the Picture This core language. *)
type color =
  | Black
  | White
  | Gray
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Purple
  | Pink
  | Brown
  | Navy
  | Teal
  | Gold
  | Cream

(** A geometric transformation. *)
type transformation =
  | Translate of float * float
  | Rotate of float
  | Scale of float

(** A drawable primitive or recursively nested compound element. *)
type element =
  | Circle of {
      center_x : float;
      center_y : float;
      radius : float;
      fill : color;
    }
  | Rectangle of {
      x : float;
      y : float;
      width : float;
      height : float;
      fill : color;
    }
  | Line of {
      x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      stroke : color;
      stroke_width : float;
    }
  | Text of {
      x : float;
      y : float;
      size : float;
      fill : color;
      contents : string;
    }
  | Transform of transformation * element list
  | Repeat of int * transformation * element list

type picture = {
  width : float;
  height : float;
  background : color option;
  elements : element list;
}
(** A complete picture, including its canvas and ordered elements. *)
