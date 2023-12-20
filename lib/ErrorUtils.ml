open TAst

let pp_error_head ppf (pos : Ast.position) =
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

let warning txt (pos : Ast.position option) =
  (match pos with
  | None -> ()
  | Some pos -> Format.eprintf "%a@." pp_error_head pos);
  Format.eprintf "%a: %s@." (pp_severity "33") "Warning" txt

let type_error txt (pos : Ast.position option) =
  (match pos with
  | None -> ()
  | Some pos -> Format.eprintf "%a@." pp_error_head pos);
  Format.eprintf "%a: %s@." (pp_severity "31") "Error" txt;
  exit 1

let rec pp_slist f ppf l =
  match l with
  | [] -> assert false
  | [ x; y ] -> Format.fprintf ppf "%s and %s" (f x) (f y)
  | hd :: tl -> Format.fprintf ppf "%s, %a" (f hd) (pp_slist f) tl

let pp_slist f ppf l =
  match l with
  | [] -> Format.pp_print_string ppf ""
  | [ x ] -> Format.pp_print_string ppf (f x)
  | l -> pp_slist f ppf l

let pp_sset ppf s = (pp_slist Fun.id) ppf (SSet.elements s)
let pp_smap ppf s = (pp_slist fst) ppf (SMap.bindings s)
