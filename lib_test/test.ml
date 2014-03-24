
let Itv_tree = Interval_tree

let of_pairs pairs =
  interval_tree
    (L.fold_left
       (fun acc (a, b) -> (new_interval a b) :: acc)
       [] pairs)

let tree = interval_tree_of_pairs [ 0.,  4.;
                                    1.,  2.;
                                    3.,  9.;
                                    5.,  7.;
                                    6., 13.;
                                    8., 11.;
                                   10., 12.];;
query tree 0.5;;
