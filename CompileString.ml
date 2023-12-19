open Ast

module StyleSet = Set.Make (struct
  type t = Ast.text_style

  let compare = Stdlib.compare
end)

type str = bytes

let int_of_color = function
  | Black -> assert false
  | Red -> assert false
  | Green -> assert false
  | Yellow -> assert false
  | Blue -> assert false
  | Magenta -> assert false
  | Cyan -> assert false
  | White -> assert false
  | BrightBlack -> assert false
  | BrightRed -> assert false
  | BrightGreen -> assert false
  | BrightYellow -> assert false
  | BrightBlue -> assert false
  | BrightMagenta -> assert false
  | BrightCyan -> assert false
  | BrightWhite -> assert false

let int_of_style = function
  | Bold -> Int.shift_left 1 0
  | Faint -> Int.shift_left 1 1
  | Italic -> Int.shift_left 1 2
  | Underline -> Int.shift_left 1 3
  | Blinking -> Int.shift_left 1 4
  | Hide -> Int.shift_left 1 5
  | Crossed -> Int.shift_left 1 6
  | Overline -> Int.shift_left 1 7
  | Default -> 0

let int_of_styles s =
  let style_set =
    List.fold_right
      (fun s acc -> if s = Default then StyleSet.empty else StyleSet.add s acc)
      s StyleSet.empty
  in
  StyleSet.fold (fun s acc -> int_of_style s lor acc) style_set 0

let rec of_text tc bc st_l t =
  match t.v with
  | Concat (left, right) -> of_text tc bc st_l left @ of_text tc bc st_l right
  | TextColor (tc, t) -> of_text tc bc st_l t
  | BackColor (bc, t) -> of_text tc bc st_l t
  | Style (st, t) -> of_text tc bc (st :: st_l) t
  | Text txt ->
      String.fold_right
        (fun chr acc ->
          let v = Char.code chr in
          let v = v in
          v :: acc)
        txt []
