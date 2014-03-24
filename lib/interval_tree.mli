
type t

module Interval : sig

  (** An interval has a left and a right bound *)
  type t = { lbound : float ;
             rbound : float }

  (** [create lbound rbound] create a new float interval.
      PRECONDITION: [lbound] must be <= [rbound]. *)
  val create : float -> float -> t

  (** [of_pair (lbound, rbound)] = [create lbound rbound] *)
  val of_pair : float * float -> t

  (** [to_pair itv] = [(lbound, rbound)] *)
  val to_pair : t -> float * float
end

(** {4 Constructors} *)

(** [create intervals_list] : interval tree of all intervals in the list *)
val create : Interval.t list -> t

(** [of_pairs intervals_bounds_pairs] : interval tree of all intervals
    whose bounds are given in a list of bounds pairs *)
val of_pairs : (float * float) list -> t

(** {4 Query} *)

(** [query tree q] : list of intervals in the tree [t] containing
    the float [q] *)
val query : t -> float -> Interval.t list
