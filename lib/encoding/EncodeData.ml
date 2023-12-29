open Ast
open TAst
open EncodedFile
open Labels
open Integers
open ErrorUtils
open PositionUtils
open SplitFile
open EncodingCommon

let add_section cur_pos sec_label sec_size =
  match MemoryAddress.next cur_pos with
  | Some i ->
      i
  | None ->
      let txt =
        Format.sprintf
          "The section '%s' of size %d, is too long to be in a 32bit address \
           space."
          (DataLabel.name sec_label) sec_size
      in
      error txt (DataLabel.position sec_label)

(** [encode_text s] : Encode [s] with the encoding scheme.*)
let encode_text t =
  Monoid.fold_left
    (fun (text_size, text_bytes) t ->
      String.fold_left
        (fun (size, acc) chr ->
          let word =
            if chr = '\000' then Word.zero
            else
              let code = (0, Word.zero) in
              (* Compute the ASCII Code of chr *)
              let code = encode_ascii code chr in
              (* Add the text color *)
              let code = encode_color code t.text_color in
              (* Add the background color *)
              let code = encode_color code t.back_color in
              (* Add the text style *)
              let code = encode_style code t.style in
              snd code
          in
          (size + 1, Monoid.(acc @@ of_elm word)) )
        (text_size, text_bytes) t.text )
    (0, Monoid.empty) t
(* in
   (text_size, text_bytes) *)

(** [process_data data] : Convert the data declaration [data] to a well-formed
       one. *)
let process_data data =
  match data.v with Str text -> TString (encode_text text) | Int i -> TInt i.v

let encode_section sec =
  let size, bytes_mon =
    Monoid.fold_left
      (fun (cur_pos, data) i ->
        match process_data i with
        | TString (size, str) ->
            (cur_pos + size, Monoid.(data @@ str))
        | TInt imm ->
            let b = IntConstant.to_word imm in
            (cur_pos + 1, Monoid.(data @@ of_elm b)) )
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
        else DataLabel.fresh l.v l.pos )
      f.data
  in
  let data_next_address, data_bytes, data_label_position =
    Monoid.fold_left
      (fun (cur_pos, data, lbl_pos) sec ->
        let sec_label, sec_insts = sec.v in
        let sec_size, sec_bytes = encode_section sec_insts in
        let lbl_pos = DataLabel.Map.add sec_label cur_pos lbl_pos in
        let cur_pos = add_section cur_pos sec_label sec_size in
        (cur_pos, Monoid.(data @@ sec_bytes), lbl_pos) )
      (MemoryAddress.first, Monoid.empty, DataLabel.Map.empty)
      data_decls
  in
  {data_bytes; data_label_mapping; data_label_position; data_next_address}
