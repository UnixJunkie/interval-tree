
module Itvt = Interval_tree
module Itv  = Itvt.Interval
module L    = List

let sort_intervals l =
  L.sort
    (fun ((lb1 : float), _rb1, _v1) ((lb2 : float), _rb2, _v2) -> compare lb1 lb2)
    l

let main () =
  (* the values are the index of each interval in the list *)
  let interval_bounds = [ 0.,   4., 0 ;
                          1.,   2., 1 ;
                          3.,   9., 2 ;
                          5.,   7., 3 ;
                          6.,  13., 4 ;
                          8.,  11., 5 ;
                          10., 12., 6 ] in
  let tree = Itvt.of_triplets interval_bounds in
  let test q =
    sort_intervals (L.map Itv.to_triplet (Itvt.query tree q))
  in
  assert([(0., 4. , 0)]                              = test 0.5 );
  assert([(0., 4. , 0); (1., 2. , 1)]                = test 1.0 );
  assert([(0., 4. , 0); (3., 9. , 2)]                = test 3.0 );
  assert([(3., 9. , 2); (5., 7. , 3)]                = test 5.0 );
  assert([(3., 9. , 2); (5., 7. , 3); (6., 13. , 4)] = test 6.0 );
  assert([(3., 9. , 2); (6., 13., 4); (8., 11. , 5)] = test 8.0 );
  assert([(6., 13., 4); (8., 11., 5); (10., 12., 6)] = test 10.0);

  assert(interval_bounds = sort_intervals (Itvt.to_triplets tree))

let () = main ()
