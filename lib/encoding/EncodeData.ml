open Ast
open TAst
open EncodeCommon

module StyleSet = Set.Make (struct
  type t = Ast.text_style

  let compare = Stdlib.compare
end)

(** [int_of_color c] : Convert the color [c] to its corresponding value.*)
let int_of_color = function
  | Black -> 0l
  | Red -> 1l
  | Green -> 2l
  | Yellow -> 3l
  | Blue -> 4l
  | Magenta -> 5l
  | Cyan -> 6l
  | White -> 7l
  | BrightBlack -> 8l
  | BrightRed -> 9l
  | BrightGreen -> 10l
  | BrightYellow -> 11l
  | BrightBlue -> 12l
  | BrightMagenta -> 13l
  | BrightCyan -> 14l
  | BrightWhite -> 15l

(** [int_of_style s] : Convert the style [s] to its corresponding value.*)
let int_of_style = function
  | Bold -> Int32.shift_left 1l 0
  | Faint -> Int32.shift_left 1l 1
  | Italic -> Int32.shift_left 1l 2
  | Underline -> Int32.shift_left 1l 3
  | Blinking -> Int32.shift_left 1l 4
  | Hide -> Int32.shift_left 1l 5
  | Crossed -> Int32.shift_left 1l 6
  | Overline -> Int32.shift_left 1l 7
  | Default -> 0l

(** [of_text s] : Encode [s] with the encoding scheme.*)
let of_text t =
  let rec of_text tc bc sts t =
    match t with
    | Concat (left, right) ->
        let left_size, left_str = of_text tc bc sts left in
        let right_size, right_str = of_text tc bc sts right in
        ( Int32.add left_size right_size,
          Bytes.concat Bytes.empty [ left_str; right_str ] )
    | TextColor (tc, t) -> of_text tc bc sts t
    | BackColor (bc, t) -> of_text tc bc sts t
    | Style (Default, t) -> of_text tc bc StyleSet.empty t
    | Style (st, t) -> of_text tc bc (StyleSet.add st sts) t
    | Text txt ->
        let text_size, text_bytes =
          String.fold_right
            (fun chr (size, acc) ->
              (* Compute the ASCII Code of chr *)
              let v = Int32.of_int (Char.code chr) in
              (* Add the text color *)
              let v =
                let tcol = int_of_color tc in
                Int32.logor v (Int32.shift_left tcol 8)
              in
              (* Add the background color *)
              let v =
                let tcol = int_of_color bc in
                Int32.logor v (Int32.shift_left tcol 12)
              in
              (* Add the text style *)
              let v =
                let int_style =
                  StyleSet.fold
                    (fun s acc -> Int32.logor (int_of_style s) acc)
                    sts 0l
                in
                Int32.logor v (Int32.shift_left int_style 16)
              in
              (* Pack everything in bytes *)
              let b = Bytes.create 4 in
              Bytes.set_int32_be b 0 v;
              (Int32.add size 1l, b :: acc))
            txt (0l, [])
        in
        (text_size, Bytes.concat Bytes.empty text_bytes)
  in
  of_text default_text_color default_background_color StyleSet.empty t

(** [process_data data] : Convert the data declaration [data] to a well-formed
    one. *)
let process_data data =
  match data.v with
  | Ascii text -> TString (of_text text)
  | Str text -> TString (of_text (Concat (text, Text "\000")))
  | Int i -> TInt (check_immediate i)

let encode_section sec =
  let size, bytes_list =
    List.fold_left
      (fun (cur_pos, data) i ->
        match process_data i with
        | TString (size, str) -> (Int32.add cur_pos size, data @ [ str ])
        | TInt i ->
            let b = Bytes.create 4 in
            Bytes.set_int32_be b 0 i;
            (Int32.add cur_pos 1l, data @ [ b ]))
      (0l, []) sec
  in
  (size, Bytes.concat Bytes.empty bytes_list)

let encode_data (f : file) =
  let data_decls, _ = split_by_label f.data in
  let size, data, mapping =
    List.fold_left
      (fun (cur_pos, data, mapping) (sec_name, sec) ->
        let sec_size, sec_bytes = encode_section sec in
        let mapping = SMap.add sec_name cur_pos mapping in
        let cur_pos = Int32.add cur_pos sec_size in
        (cur_pos, data @ [ sec_bytes ], mapping))
      (0l, [], SMap.empty) data_decls
  in
  let data = Bytes.concat Bytes.empty data in
  { data; size; mapping }
