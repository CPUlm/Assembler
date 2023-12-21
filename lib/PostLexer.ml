open Parser

let rec gen_token =
  let tokens = Queue.create () in
  fun lb ->(
    if Queue.is_empty tokens then (
      let a = Lexer.next_token lb in
      Queue.add a tokens
    );
    let t = Queue.pop tokens in
    match t with
    | INCLUDE -> (
      if Queue.is_empty tokens then(
        let a = Lexer.next_token lb in
        Queue.add a tokens);
      match (Queue.pop tokens) with 
      | STR fname -> (
          let f = open_in fname in
          let f_lb = Lexing.from_channel f in
          let rec lex_tokens acc =
            (
            match gen_token f_lb with
            | EOF -> List.rev acc
            | tokens -> lex_tokens ([tokens] @ acc)
            )
          in
          let r = lex_tokens [] in 
          let _ = close_in f in
          List.iter (fun x -> Queue.add x tokens ) r;
          Queue.pop tokens
      )
      | _ -> raise (Lexer.Lexing_error "using include without a string after it")
    )
    | _ -> t
)
(*
match sl with
    | [STR fname] -> (
      let f =  open_in fname in
      let f_lb = Lexing.from_channel f in
      let rec lex_tokens acc =
        match next_token f_lb with
        | [EOF] -> List.rev acc
        | tokens -> lex_tokens (tokens @ acc)
      in
      let r = lex_tokens [] in 
      let _ = close_in f in
      r
    )
    *)