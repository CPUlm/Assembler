(** A small module to concatenate things quickly *)
type 'a t = Empty | LeafL of 'a list | LeafElm of 'a | Concat of 'a t * 'a t

(** The empty set. *)
let empty = Empty

(** Build a monoid from of list. *)
let from_list x = LeafL x

(** Build a set from of list. *)
let from_elm x = LeafElm x

(** Concatenate two set. *)
let ( @@ ) a b =
  match (a, b) with Empty, t | t, Empty -> t | a, b -> Concat (a, b)

(** Iterate of the set *)
let rec fold (f : 'acc -> 'a -> 'acc) acc = function
  | Empty -> acc
  | LeafL x -> List.fold_left f acc x
  | LeafElm x -> f acc x
  | Concat (left, right) ->
      let left_acc = fold f acc left in
      fold f left_acc right

(** Convert a monoid to a list *)
let to_list x = fold (fun acc x -> x :: acc) [] x |> List.rev
