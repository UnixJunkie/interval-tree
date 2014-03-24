(* interval tree for float intervals, intended usage is to build the tree once
   then query it many times

   reference:

   @book{ CompGeomThirdEdSpringer,
   title     = "Computational Geometry: Algorithms and Applications",
   author    = "M. {de Berg} and O. Cheong and M. {van Kreveld} and
   M. Overmars",
   edition   = "Third Edition",
   pages     = {223--224},
   doi       = "10.1007/978-3-540-77974-2",
   year      = "2008",
   publisher = "Springer"
   } *)

module Interval : sig
  (* since bounds are read-only, we don't care about leaving them public *)
  type t = { lbound : float ;
             rbound : float }
  val create : float -> float -> t
  val of_pair : float * float -> t
  val to_pair : t -> float * float
end = struct
  type t = { lbound : float ;
             rbound : float }
  let create l r =
    assert (l <= r);
    { lbound = l ;
      rbound = r }
  let of_pair (lb, rb) =
    create lb rb
  let to_pair itv =
    (itv.lbound, itv.rbound)
end

module A   = Array
module Itv = Interval
module L   = List

open Itv

type interval_tree =
  | Empty
  | Node of
      (* x_mid left_list    right_list   left_tree       right_tree *)
      float *  Itv.t list * Itv.t list * interval_tree * interval_tree

type t = interval_tree

(* -------------------- utility functions -------------------- *)

let leftmost_bound_first i1 i2 =
  compare i1.lbound i2.lbound

let rightmost_bound_first i1 i2 =
  compare i2.rbound i1.rbound

let is_before interval x_mid =
  interval.rbound < x_mid

let contains interval x_mid =
  (interval.lbound <= x_mid) && (x_mid <= interval.rbound)

let bounds_array_of_intervals intervals =
  let n   = L.length intervals  in
  let res = A.create (2 * n) 0. in
  let i   = ref 0               in
  L.iter
    (fun interval ->
      res.(!i) <- interval.lbound; incr i;
      res.(!i) <- interval.rbound; incr i)
    intervals;
  res

let median xs =
  A.sort compare xs;
  let n = A.length xs in
  if n mod 2 = 1 then
    xs.(n/2)
  else
    (xs.(n/2) +. xs.(n/2 - 1)) /. 2.0

let median intervals =
  let bounds = bounds_array_of_intervals intervals in
  median bounds

let partition intervals x_mid =
  let left_intervals, maybe_right_intervals =
    L.partition
      (fun interval -> is_before interval x_mid)
      intervals in
  let mid_intervals, right_intervals =
    L.partition
      (fun interval -> contains interval x_mid)
      maybe_right_intervals in
  left_intervals, mid_intervals, right_intervals

(* -------------------- construction -------------------- *)

(* interval tree of a list of intervals
   WARNING: NOT TAIL REC. *)
let rec create = function
  | [] -> Empty
  | intervals ->
    let x_mid            = median intervals                 in
    let left, mid, right = partition intervals x_mid        in
    let left_list        = L.sort leftmost_bound_first  mid in
    let right_list       = L.sort rightmost_bound_first mid in
    Node (x_mid,
          left_list, right_list,
          create left, create right)

(* interval tree of a list of interval bounds pairs
   [(lb1, rb1); (lb2, rb2); ...]
   WARNING: NOT TAIL REC. *)
let of_pairs pairs =
  create
    (L.fold_left
       (fun acc (a, b) -> (Itv.create a b) :: acc)
       []
       pairs)

(* -------------------- query -------------------- *)

(* fold_left f on l while p is true *)
let rec fold_while f p acc = function
  | [] -> acc
  | x :: xs ->
    if p x then
      fold_while f p (f x :: acc) xs
    else
      acc

let filter_left_list l qx acc =
  fold_while
    (fun x -> x)
    (fun interval -> interval.lbound <= qx)
    acc l

let filter_right_list l qx acc =
  fold_while
    (fun x -> x)
    (fun interval -> interval.rbound >= qx)
    acc l

(* find all intervals that contain qx *)
let query initial_tree qx =
  let rec query_priv acc = function
    | Empty -> acc
    | Node (x_mid, left_list, right_list, left_tree, right_tree) ->
      if qx < x_mid then
        let new_acc = filter_left_list  left_list  qx acc in
        query_priv new_acc left_tree
      else
        let new_acc = filter_right_list right_list qx acc in
        query_priv new_acc right_tree
  in
  query_priv [] initial_tree
