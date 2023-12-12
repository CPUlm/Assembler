type 'a t

val empty : 'a t
(** The empty set. *)

val from_list : 'a list -> 'a t
(** Build a monoid from of list. *)

val from_elm : 'a -> 'a t
(** Build a set from of list. *)

val ( @@ ) : 'a t -> 'a t -> 'a t
(** Concatenate two set. *)

val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** Iterate of the set *)

val to_list : 'a t -> 'a list
(** Convert a monoid to a list, concerve the order. *)
