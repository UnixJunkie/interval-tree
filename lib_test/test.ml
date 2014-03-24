
module Itvt = Interval_tree
module Itv  = Itvt.Interval
module L    = List

let tree = Itvt.of_pairs [ 0.,  4.;
                           1.,  2.;
                           3.,  9.;
                           5.,  7.;
                           6., 13.;
                           8., 11.;
                           10., 12.];;
Itvt.query tree 0.5;;

(* FBR:
 - to_pairs
*)
