open Ast
open TAst
open EncodingStruct
open Labels
open Integers
open ErrorUtils
open PositionUtils
open SplitFile

let add_section cur_pos sec_label sec_size =
  match MemoryAddress.next cur_pos with
  | Some i -> i
  | None ->
      let txt =
        Format.sprintf
          "The section '%s' of size %d, is too long to be in a 32bit address \
           space."
          (DataLabel.name sec_label) sec_size
      in
      error txt (DataLabel.position sec_label)

module StyleSet = Set.Make (struct
  type t = Ast.text_style

  let compare = Stdlib.compare
end)

(** Encode a character *)
let encode_char (pos, v) chr =
  let char_size = 8 in
  let c = Char.code chr in
  (pos + char_size, Word.add_at v (Int32.of_int c) pos)

(** Encode a color *)
let encode_color (pos, v) col =
  let color_size = 4 in
  let c =
    match col with
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
  in
  (pos + color_size, Word.add_at v (Int32.of_int c) pos)

(** Encode a style set *)
let encode_style (pos, v) style =
  let style_size = 4 in
  let c =
    StyleSet.fold
      (fun style_piece acc ->
        match style_piece with
        | Bold -> Int32.(logor acc (shift_left one 0))
        | Faint -> Int32.(logor acc (shift_left one 1))
        | Italic -> Int32.(logor acc (shift_left one 2))
        | Underline -> Int32.(logor acc (shift_left one 3))
        | Blinking -> Int32.(logor acc (shift_left one 4))
        | Hide -> Int32.(logor acc (shift_left one 5))
        | Crossed -> Int32.(logor acc (shift_left one 6))
        | Overline -> Int32.(logor acc (shift_left one 7))
        | Default -> 0l)
      style 0l
  in
  (pos + style_size, Word.add_at v c pos)

let get_code = snd

(** [of_text s] : Encode [s] with the encoding scheme.*)
let of_text t =
  let rec of_text tc bc sts t =
    match t with
    | Concat (left, right) ->
        let left_size, left_str = of_text tc bc sts left in
        let right_size, right_str = of_text tc bc sts right in
        (left_size + right_size, Monoid.(left_str @@ right_str))
    | TextColor (tc, t) -> of_text tc bc sts t
    | BackColor (bc, t) -> of_text tc bc sts t
    | Style (Default, t) -> of_text tc bc StyleSet.empty t
    | Style (st, t) -> of_text tc bc (StyleSet.add st sts) t
    | Text txt ->
        let code = (0, Word.zero) in
        let text_size, text_bytes =
          String.fold_left
            (fun (size, acc) chr ->
              (* Compute the ASCII Code of chr *)
              let code = encode_char code chr in
              (* Add the text color *)
              let code = encode_color code tc in
              (* Add the background color *)
              let code = encode_color code bc in
              (* Add the text style *)
              let code = encode_style code sts in
              (* Get the word *)
              let word = get_code code in
              (size + 1, Monoid.(acc @@ of_elm word)))
            (0, Monoid.empty) txt
        in
        (text_size, text_bytes)
  in
  of_text default_text_color default_background_color StyleSet.empty t

(** [process_data data] : Convert the data declaration [data] to a well-formed
       one. *)
let process_data data =
  match data.v with Str text -> TString (of_text text) | Int i -> TInt i.v

let encode_section sec =
  let size, bytes_mon =
    Monoid.fold_left
      (fun (cur_pos, data) i ->
        match process_data i with
        | TString (size, str) -> (cur_pos + size, Monoid.(data @@ str))
        | TInt imm ->
            let b = Immediate.to_word imm in
            (cur_pos + 1, Monoid.(data @@ of_elm b)))
      (0, Monoid.empty) sec
  in
  (size, bytes_mon)

let encode_data (f : file) =
  let data_decls, data_label_mapping =
    split_by_label
      (fun l ->
        if l.v = "main" then
          let txt =
            Format.asprintf "The label 'main' cannot be used as a data label."
          in
          error txt l.pos
        else DataLabel.fresh l.v l.pos)
      f.data
  in
  let _, data_bytes, data_label_position =
    Monoid.fold_left
      (fun (cur_pos, data, lbl_pos) sec ->
        let sec_label, sec_insts = sec.v in
        let sec_size, sec_bytes = encode_section sec_insts in
        let lbl_pos = DataLabel.Map.add sec_label cur_pos lbl_pos in
        let cur_pos = add_section cur_pos sec_label sec_size in
        (cur_pos, Monoid.(data @@ sec_bytes), lbl_pos))
      (MemoryAddress.first, Monoid.empty, DataLabel.Map.empty)
      data_decls
  in
  { data_bytes; data_label_mapping; data_label_position }
