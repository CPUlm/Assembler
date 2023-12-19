open Ast

module StyleSet = Set.Make (struct
  type t = Ast.text_style

  let compare = Stdlib.compare
end)

type str = bytes

(** Convert a [color] type to its corresponding value.*)
let int_of_color = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
  | BrightBlack -> 8
  | BrightRed -> 9
  | BrightGreen -> 10
  | BrightYellow -> 11
  | BrightBlue -> 12
  | BrightMagenta -> 13
  | BrightCyan -> 14
  | BrightWhite -> 15

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

let int_of_styles s = StyleSet.fold (fun s acc -> int_of_style s lor acc) s 0

let rec of_text tc bc sts t =
  match t.v with
  | Concat (left, right) -> of_text tc bc sts left @ of_text tc bc sts right
  | TextColor (tc, t) -> of_text tc bc sts t
  | BackColor (bc, t) -> of_text tc bc sts t
  | Style (Default, t) -> of_text tc bc StyleSet.empty t
  | Style (st, t) -> of_text tc bc (StyleSet.add st sts) t
  | Text txt ->
      String.fold_right
        (fun chr acc ->
          let v = Char.code chr in
          let v = v in
          v :: acc)
        txt []
