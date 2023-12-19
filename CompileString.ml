open Ast

type str = bytes

let of_text t =
  match t.v with
  | Concat _ -> assert false
  | TextColor _ -> assert false
  | BackColor _ -> assert false
  | Style _ -> assert false
