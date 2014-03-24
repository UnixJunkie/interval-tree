
module Itvt = Interval_tree
module Itv  = Itvt.Interval
module L    = List

let of_pairs pairs =
  Itvt.create
    (L.fold_left
       (fun acc (a, b) -> (Itv.create a b) :: acc)
       [] pairs)

let tree = of_pairs [ 0.,  4.;
                      1.,  2.;
                      3.,  9.;
                      5.,  7.;
                      6., 13.;
                      8., 11.;
                      10., 12.];;
Itvt.query tree 0.5;;

(* FBR:
 - of_pairs
 - to_pairs
*)
