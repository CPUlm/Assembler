type 'a t

val empty : 'a t
(** The empty monoid. *)

val of_list : 'a list -> 'a t
(** Build a monoid from of list. *)

val of_elm : 'a -> 'a t
(** Build a set from of list. *)

val ( @@ ) : 'a t -> 'a t -> 'a t
(** Concatenate two monoid . *)

val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** Iterate of the monoid *)

val to_list : 'a t -> 'a list
(** Convert a monoid to a list, concerve the order. *)