(** True if warnings should be treated as errors. *)
let fatal_warnings = ref false

let pp_pos ppf (pos : PositionUtils.position) =
  if pos <> PositionUtils.dummy_pos then
    Format.fprintf ppf "%s:%i-%i:%i-%i" pos.file pos.beg_line pos.end_line
      pos.beg_col pos.end_col

let pp_error_head ppf (pos : PositionUtils.position) =
  if pos <> PositionUtils.dummy_pos then
    let begin_col, end_col = (pos.beg_col + 1, pos.end_col + 1) in
    if pos.beg_line = pos.end_line then
      Format.fprintf ppf "\x1b[1mFile \"%s\", line %i, characters %i-%i:\x1b[0m"
        pos.file pos.beg_line begin_col end_col
    else
      Format.fprintf ppf
        "\x1b[1mFile \"%s\", lines %i-%i, characters %i-%i:\x1b[0m" pos.file
        pos.beg_line pos.end_line begin_col end_col

let pp_severity color ppf severity =
  Format.fprintf ppf "\x1b[1;%sm%s\x1b[0m" color severity

let error txt (pos : PositionUtils.position) =
  Format.eprintf "%a@.%a: %s@." pp_error_head pos (pp_severity "31") "Error" txt ;
  exit 1

let file_error txt =
  Format.eprintf "%a: %s@." (pp_severity "31") "Error" txt ;
  exit 1

let warning txt (pos : PositionUtils.position) =
  if !fatal_warnings then error (txt ^ " (promoted warning)") pos
  else
    Format.eprintf "%a@.%a: %s@." pp_error_head pos (pp_severity "33") "Warning"
      txt

let file_warning txt =
  if !fatal_warnings then file_error (txt ^ " (promoted warning)")
  else Format.eprintf "%a: %s@." (pp_severity "33") "Warning" txt

let rec pp_slist f ppf l =
  match l with
  | [] ->
      ()
  | [x; y] ->
      Format.fprintf ppf "%s and %s" (f x) (f y)
  | hd :: tl ->
      Format.fprintf ppf "%s, %a" (f hd) (pp_slist f) tl

let pp_slist f ppf l =
  match l with
  | [] ->
      Format.pp_print_string ppf ""
  | [x] ->
      Format.pp_print_string ppf (f x)
  | l ->
      pp_slist f ppf l
