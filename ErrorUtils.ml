open TAst

let pp_error_head ppf (pos : Ast.position) =
  let begin_col, end_col = (pos.beg_col + 1, pos.end_col + 1) in
  if pos.beg_line = pos.end_line then
    Format.fprintf ppf "File \"%s\", line %i, characters %i-%i:" pos.file
      pos.beg_line begin_col end_col
  else
    Format.fprintf ppf "File \"%s\", line %i-%i, characters %i-%i:" pos.file
      pos.beg_line pos.end_line begin_col end_col

let warning txt (pos : Ast.position option) =
  match pos with
  | None -> Format.eprintf "Warning: %s@." txt
  | Some pos -> Format.eprintf "%a@.Warning: %s@." pp_error_head pos txt

let type_error txt (pos : Ast.position option) =
  (match pos with
  | None -> Format.eprintf "%s@." txt
  | Some pos -> Format.eprintf "%a@.%s@." pp_error_head pos txt);
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

let pp_sset ppf s = (pp_slist Fun.id) ppf (SSet.to_list s)
let pp_smap ppf s = (pp_slist fst) ppf (SMap.to_list s)
