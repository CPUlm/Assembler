open Parser

let resolve_include_fname fname current_filename =
  (Filename.dirname current_filename) ^ "/" ^ fname

let tokens_until_endmacro lb token_queue =
  let tokens = Queue.create () in
  let rec loop lb tokens =
    let token = if Queue.is_empty token_queue then
      Lexer.next_token lb
    else
      Queue.pop token_queue in
    match token with
    | ENDMACRO -> tokens
    | EOF -> raise (Lexer.Lexing_error "Unterminated macro definition, missing a .endmacro directive.")
    | _ ->
      Queue.add token tokens;
      loop lb tokens
    in loop lb tokens

let lex_token_if_not_empty lb tokens =
  if Queue.is_empty tokens then Queue.add (Lexer.next_token lb) tokens

let rec next_token =
  let macros = Hashtbl.create 17 in
  let tokens = Queue.create () in
  fun lb ->(
    if Queue.is_empty tokens then (
      let a = Lexer.next_token lb in
      Queue.add a tokens
    );
    let t = Queue.pop tokens in
    match t with
    | INCLUDE -> (
      lex_token_if_not_empty lb tokens;
      match Queue.pop tokens with 
      | STR fname -> (
        let current_filename = (Lexing.lexeme_start_p lb).pos_fname in
        let resolved_fname = resolve_include_fname fname current_filename in
        let f = try open_in resolved_fname
        with _ -> (raise (Lexer.Lexing_error ("Failed to open file '" ^ fname ^ "'.")))
        in
        let f_lb = Lexing.from_channel f in
        Lexing.set_filename f_lb fname;
        let rec lex_tokens acc =
          (
          match next_token f_lb with
          | EOF -> List.rev acc
          | tokens -> lex_tokens ([tokens] @ acc)
          )
        in
        let r = lex_tokens [] in 
        let _ = close_in f in
        List.iter (fun x -> Queue.add x tokens ) r;
        Queue.pop tokens
      )
      | _ -> raise (Lexer.Lexing_error "Using include without a string (a string should be surrounded by \"\") after it.")
    )
    | IDENT i -> (
      match Hashtbl.find_opt macros i with
      | None -> (* not a macro *) t
      | Some macro_tokens ->
        Printf.printf "MACRO FOUND %s\n" i;
        Queue.transfer macro_tokens tokens;
        lex_token_if_not_empty lb tokens;
        Queue.pop tokens
    )
    | MACRO -> (
      lex_token_if_not_empty lb tokens;
      match Queue.pop tokens with
      | IDENT mname -> (
        let macro_tokens = tokens_until_endmacro lb tokens in
        Hashtbl.add macros mname macro_tokens;
        lex_token_if_not_empty lb tokens;
        Queue.pop tokens
      )
      | _ -> raise (Lexer.Lexing_error "Expected an identifier after a .macro directive.")
    )
    | _ -> t
)