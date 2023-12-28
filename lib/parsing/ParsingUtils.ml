open Isa
open Ast
open Integers
open PositionUtils

let int_to_reg loc i =
  let pos = lexloc_to_pos loc in
  let r =
    match i with
    | 0 ->
        R0
    | 1 ->
        R1
    | 2 ->
        R2
    | 3 ->
        R3
    | 4 ->
        R4
    | 5 ->
        R5
    | 6 ->
        R6
    | 7 ->
        R7
    | 8 ->
        R8
    | 9 ->
        R9
    | 10 ->
        R10
    | 11 ->
        R11
    | 12 ->
        R12
    | 13 ->
        R13
    | 14 ->
        R14
    | 15 ->
        R15
    | 16 ->
        R16
    | 17 ->
        R17
    | 18 ->
        R18
    | 19 ->
        R19
    | 20 ->
        R20
    | 21 ->
        R21
    | 22 ->
        R22
    | 23 ->
        R23
    | 24 ->
        R24
    | 25 ->
        R25
    | 26 ->
        R26
    | 27 ->
        R27
    | _ ->
        let txt = Format.sprintf "Unknown register r%d." i in
        ErrorUtils.error txt pos
  in
  mk_pos pos r

let str_to_col =
  let colors = Hashtbl.create 11 in
  List.iter
    (fun (s, l) -> Hashtbl.add colors s l)
    [ ("black", Black)
    ; ("red", Red)
    ; ("green", Green)
    ; ("yellow", Yellow)
    ; ("blue", Blue)
    ; ("magenta", Magenta)
    ; ("cyan", Cyan)
    ; ("white", White)
    ; ("brightblack", BrightBlack)
    ; ("brightred", BrightRed)
    ; ("brightgreen", BrightGreen)
    ; ("brightyellow", BrightYellow)
    ; ("brightblue", BrightBlue)
    ; ("brightmagenta", BrightMagenta)
    ; ("brightcyan", BrightCyan)
    ; ("brightwhite", BrightWhite)
    ; ("default", Default) ] ;
  fun loc s ->
    match Hashtbl.find_opt colors s with
    | Some c ->
        c
    | None ->
        let pos = lexloc_to_pos loc in
        let txt = Format.sprintf "Unknown color '%s'." s in
        ErrorUtils.error txt pos

type parsed_text =
  | Concat of parsed_text * parsed_text
  | TextColor of color * parsed_text
  | BackColor of color * parsed_text
  | Style of text_style * parsed_text
  | Text of string

let rec mk_style_text tc bc sts = function
  | Concat (t1, t2) ->
      AstConcat (mk_style_text tc bc sts t1, mk_style_text tc bc sts t2)
  | TextColor (col, t) ->
      mk_style_text col bc sts t
  | BackColor (col, t) ->
      mk_style_text tc col sts t
  | Style (Default, t) ->
      mk_style_text tc bc StyleSet.empty t
  | Style (s, t) ->
      mk_style_text tc bc (StyleSet.add s sts) t
  | Text t ->
      AstText {text= t; style= sts; text_color= tc; back_color= bc}

let mk_style_text =
  mk_style_text default_text_color default_background_color StyleSet.empty

let mk_imm loc i =
  let pos = lexloc_to_pos loc in
  match IntConstant.of_int i with
  | Some i ->
      mk_pos pos i
  | None ->
      let txt =
        Format.sprintf "The value '%d' cannot be represented on 32 bit." i
      in
      ErrorUtils.error txt pos

let mk_offset loc i =
  let pos = lexloc_to_pos loc in
  match Offset.of_int i with
  | Some i ->
      mk_pos pos i
  | None ->
      let txt =
        Format.sprintf
          "The value '%d' does not represent a valid 32-bit program address \
           offset."
          i
      in
      ErrorUtils.error txt pos

let mk_prog_addr loc i =
  let pos = lexloc_to_pos loc in
  match ProgramAddress.of_int i with
  | Some i ->
      mk_pos pos i
  | None ->
      let txt =
        Format.sprintf
          "The value '%d' does not represent a valid 32-bit program address." i
      in
      ErrorUtils.error txt pos

let mk_pos loc data = {v= data; pos= lexloc_to_pos loc}
