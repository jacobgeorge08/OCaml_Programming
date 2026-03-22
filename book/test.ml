open OUnit2
open Data_and_types

let make_prod_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (prod_tr input) ~printer:string_of_int

let make_uni_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (is_unimodal input) ~printer:string_of_bool

let make_rev_test name expected input =
  name >:: fun _ ->
  assert_equal expected (rev_list input) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

let make_take_test name expected n input_lst =
  name >:: fun _ ->
  assert_equal expected (take_tr n input_lst) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

let make_drop_test name expected n input_lst =
  name >:: fun _ ->
  assert_equal expected (drop n input_lst) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

let drop_tests =
  "Drop"
  >::: [
         make_drop_test "Empty List" [] 5 [];
         make_drop_test "One Element" [] 0 [];
         make_drop_test "Drop four Element" [ 23 ] 4 [ 2; 9; 123; 12; 23 ];
       ]

let take_tests =
  "Take"
  >::: [
         make_take_test "Empty List" [] 5 [];
         make_take_test "One Element" [] 0 [ 2 ];
         make_take_test "Take four" [ 2; 9; 123; 12 ] 4 [ 2; 9; 123; 12; 23 ];
       ]

let prod_tests =
  "Product"
  >::: [
         make_prod_test "Empty List" 1 [];
         make_prod_test "One Element" 2 [ 2 ];
         make_prod_test "Two Element" 18 [ 2; 9 ];
       ]

let rev_tests =
  "Reverse"
  >::: [
         make_rev_test "Empty List" [] [];
         make_rev_test "One Element" [ 2 ] [ 2 ];
         make_rev_test "Multiple Elements" [ 18; 12; 9; 0; -23; -98 ]
           [ 18; 9; 12; -23; 0; -98 ];
       ]

let uni_tests =
  "Unimodal"
  >::: [
         make_uni_test "Empty List" true [];
         make_uni_test "Up Then Down" true [ 1; 2; 3; 4; 1 ];
         make_uni_test "Up Only" true [ 1; 2; 3; 4 ];
         make_uni_test "Down Only" true [ 5; 4; 3; 2 ];
         make_uni_test "Constant" true [ 2; 2; 2; 2 ];
         make_uni_test "Single element" true [ 2 ];
         make_uni_test "Up Down up" false [ 1; 3; 2; 4 ];
         make_uni_test "Up Down up" false [ 1; 2; 1; 2 ];
         make_uni_test "Down up" false [ 1; 2; 1; 2 ];
       ]

let list_max_tests =
  ""
  >::: [
         ( "empty" >:: fun _ ->
           assert_raises (Failure "empty") (fun () -> list_max []) );
         ("nonempty" >:: fun _ -> assert_equal 8 (list_max [ 3; 1; 4; 8 ]));
       ]

let _ =
  run_test_tt_main
    ("Final Suite"
    >::: [
           prod_tests;
           rev_tests;
           take_tests;
           drop_tests;
           uni_tests;
           list_max_tests;
         ])
