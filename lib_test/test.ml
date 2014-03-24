
module Itvt = Interval_tree
module Itv  = Itvt.Interval
module L    = List

let sort_intervals l =
  L.sort
    (fun ((lb1 : float), _rb1) ((lb2 : float), _rb2) -> compare lb1 lb2)
    l

let main () =
  let interval_bounds = [ 0.,   4.;
                          1.,   2.;
                          3.,   9.;
                          5.,   7.;
                          6.,  13.;
                          8.,  11.;
                          10., 12.] in
  let tree = Itvt.of_pairs interval_bounds in
  let test q =
    sort_intervals (L.map Itv.to_pair (Itvt.query tree q))
  in
  assert([(0., 4. )]                        = test 0.5 );
  assert([(0., 4. ); (1., 2. )]             = test 1.0 );
  assert([(0., 4. ); (3., 9. )]             = test 3.0 );
  assert([(3., 9. ); (5., 7. )]             = test 5.0 );
  assert([(3., 9. ); (5., 7. ); (6., 13. )] = test 6.0 );
  assert([(3., 9. ); (6., 13.); (8., 11. )] = test 8.0 );
  assert([(6., 13.); (8., 11.); (10., 12.)] = test 10.0);
;;

main()
