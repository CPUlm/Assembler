type position =
  {beg_col: int; beg_line: int; end_col: int; end_line: int; file: string}

type 'a pos = {v: 'a; pos: position}

let mk_pos pos x = {v= x; pos}

(** Convert the [$loc] of menhir to a position. *)
let lexloc_to_pos (pos : Lexing.position * Lexing.position) =
  let beg_p, end_p = pos in
  let file = beg_p.pos_fname in
  let beg_col = beg_p.pos_cnum - beg_p.pos_bol in
  let end_col = end_p.pos_cnum - end_p.pos_bol in
  {beg_line= beg_p.pos_lnum; beg_col; end_line= end_p.pos_lnum; end_col; file}

let label_to_pos (pos : Lexing.position * Lexing.position) l =
  let p = lexloc_to_pos pos in
  mk_pos p l

let int_to_pos (pos : Lexing.position * Lexing.position) i =
  let p = lexloc_to_pos pos in
  mk_pos p i

(** Convert the current position of the [lexbuf] to a position. *)
let lexbuf_to_pos lexbuf =
  lexloc_to_pos (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

(** Merge the beginning of [p1] with the end of [p2]. *)
let merge_pos p1 p2 =
  if p1.file <> p2.file then
    raise
      (Invalid_argument "Cannot merge position that are not in the same file")
  else {p1 with end_line= p2.end_line; end_col= p2.end_col}

(** This is the position of the end of file. *)
let eof_pos lexbuf =
  let pos = lexbuf_to_pos lexbuf in
  {pos with end_col= -1; beg_col= -1}
