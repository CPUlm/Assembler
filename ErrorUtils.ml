let pp_error_head ppf (pos : Ast.position) =
  let begin_col, end_col = (pos.beg_col + 1, pos.end_col + 1) in
  if pos.beg_line = pos.end_line then
    Format.fprintf ppf "File \"%s\", line %i, characters %i-%i:" pos.file
      pos.beg_line begin_col end_col
  else
    Format.fprintf ppf "File \"%s\", line %i-%i, characters %i-%i:" pos.file
      pos.beg_line pos.end_line begin_col end_col

let warning txt (pos : Ast.position) =
  Format.eprintf "%a@.Warning: %s@." pp_error_head pos txt

let type_error txt (pos : Ast.position) =
  Format.eprintf "%a@.%s@." pp_error_head pos txt;
  exit 1
