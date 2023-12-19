let pp_error_head ppf (pos : Ast.position) =
  let begin_col, end_col = (pos.beg_col + 1, pos.end_col + 1) in
  if pos.beg_line = pos.end_line then
    Format.fprintf ppf "\x1b[1mFile \"%s\", line %i, characters %i-%i:\x1b[0m" pos.file
      pos.beg_line begin_col end_col
  else
    Format.fprintf ppf "\x1b[1mFile \"%s\", line %i-%i, characters %i-%i:\x1b[0m" pos.file
      pos.beg_line pos.end_line begin_col end_col

  let pp_severity color ppf severity =
    Format.fprintf ppf "\x1b[1;%sm%s\x1b[0m" color severity

let warning txt (pos : Ast.position) =
  Format.eprintf "%a@.%a: %s@." pp_error_head pos (pp_severity "33") "Warning" txt

let type_error txt (pos : Ast.position) =
  Format.eprintf "%a@.%a: %s@." pp_error_head pos (pp_severity "31") "Error" txt;
  exit 1
