
module Interval : sig
  type t
  val create : float -> float -> t
  val to_pair : t -> float * float
end

type t

val create   : Interval.t list -> t
val of_pairs : (float * float) list -> t
val query    : t -> float -> Interval.t list
