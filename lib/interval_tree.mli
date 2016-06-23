
type 'a t

module Interval : sig

  (** An interval has a left bound, a right bound and a payload (called value) *)
  type 'a t = { lbound : float ;
                rbound : float ;
                value  : 'a    }

  (** [create lbound rbound value] create a new float interval
      with an associated value.
      PRECONDITION: [lbound] must be <= [rbound]. *)
  val create: float -> float -> 'a -> 'a t

  (** [of_triplet (lbound, rbound, value)] = [create lbound rbound value] *)
  val of_triplet: float * float * 'a -> 'a t

  (** [to_triplet itv] = [(lbound, rbound, value)] *)
  val to_triplet: 'a t -> float * float * 'a
end

(** {4 Constructors} *)

(** [create intervals_list]: interval tree of all intervals in the list.
    Not tail-recursive. *)
val create: 'a Interval.t list -> 'a t

(** [of_triplets interval_triplets]: interval tree of all intervals
    whose bounds and values are given in a list.
    Not tail-recursive. *)
val of_triplets: (float * float * 'a) list -> 'a t

(** list all intervals and values in the tree.
    Inverse of of_triplets.
    Not tail-recursive. *)
val to_triplets: 'a t -> (float * float * 'a) list

(** {4 Query} *)

(** [query tree q]: list of intervals in the tree [t] containing
    the float [q] *)
val query: 'a t -> float -> 'a Interval.t list
