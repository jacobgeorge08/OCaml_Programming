open OUnit2
open Data_and_types
open Higher_order

let make_prod_test name expected input =
  name >:: fun _ -> assert_equal expected (prod_tr input) ~printer:string_of_int

let make_uni_test name expected input =
  name >:: fun _ -> assert_equal expected (is_unimodal input) ~printer:string_of_bool

let make_rev_test name expected input =
  name >:: fun _ ->
  assert_equal expected (rev_list input) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

let make_row_vector_test name expected input1 input2 =
  name >:: fun _ ->
  assert_equal expected (add_row_vectors input1 input2) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

let make_take_test name expected n input_lst =
  name >:: fun _ ->
  assert_equal expected (take_tr n input_lst) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

let make_drop_test name expected n input_lst =
  name >:: fun _ ->
  assert_equal expected (drop n input_lst) ~printer:(fun l ->
      "[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

let string_of_matrix (m : int list list) : string =
  let string_of_row r = "[" ^ String.concat "; " (List.map string_of_int r) ^ "]" in
  "[" ^ String.concat "; " (List.map string_of_row m) ^ "]"

let make_matrix_test name expected input =
  name >:: fun _ ->
  assert_equal expected (is_valid_matrix input) ~printer:string_of_bool
    ~msg:("Failed on input: " ^ string_of_matrix input)

let make_add_matrix_test name expected matrix1 matrix2 =
  name >:: fun _ -> assert_equal expected (add_matrices matrix1 matrix2) ~printer:string_of_matrix

let make_multiply_matrix_test name expected m1 m2 =
  name >:: fun _ ->
  assert_equal expected (multiply_matrices m1 m2) ~printer:string_of_matrix
    ~msg:("Multiplying " ^ string_of_matrix m1 ^ " by " ^ string_of_matrix m2)

let multiply_matrix_tests =
  "Multiply Matrix"
  >::: [
         make_multiply_matrix_test "1x1" [ [ 5 ] ] [ [ 1 ] ] [ [ 5 ] ];
         make_multiply_matrix_test "2x2"
           [ [ 19; 22 ]; [ 43; 50 ] ]
           [ [ 1; 2 ]; [ 3; 4 ] ]
           [ [ 5; 6 ]; [ 7; 8 ] ];
         make_multiply_matrix_test "(2x3 * 3x2)"
           [ [ 5; 1 ]; [ 4; 2 ] ]
           [ [ 1; 0; 2 ]; [ -1; 3; 1 ] ]
           [ [ 3; 1 ]; [ 2; 1 ]; [ 1; 0 ] ];
         make_multiply_matrix_test "Zero Matrix "
           [ [ 0; 0 ]; [ 0; 0 ] ]
           [ [ 1; 2 ]; [ 3; 4 ] ]
           [ [ 0; 0 ]; [ 0; 0 ] ];
       ]

let add_matrix_tests =
  "Add Matrix"
  >::: [
         make_add_matrix_test "Single Element" [ [ 2 ] ] [ [ 1 ] ] [ [ 1 ] ];
         make_add_matrix_test "Rectangular 2x3"
           [ [ 5; 7; 9 ]; [ 11; 13; 15 ] ]
           [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
           [ [ 4; 5; 6 ]; [ 7; 8; 9 ] ];
         make_add_matrix_test "Single Column" [ [ 3 ]; [ 7 ]; [ 11 ] ] [ [ 1 ]; [ 2 ]; [ 3 ] ]
           [ [ 2 ]; [ 5 ]; [ 8 ] ];
         make_add_matrix_test "Add Zero Matrix"
           [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
           [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
           [ [ 0; 0; 0 ]; [ 0; 0; 0 ] ];
       ]

let row_vector_tests =
  "Row Vector"
  >::: [
         make_row_vector_test "Empty List" [] [] [];
         make_row_vector_test "Standard Addition" [ 4; 7; 9; 14 ] [ 2; 4; 5; 9 ] [ 2; 3; 4; 5 ];
       ]

let is_valid_tests =
  "Valid Matrix"
  >::: [
         make_matrix_test "Empty Matrix" false [];
         make_matrix_test "Single Element" true [ [ 1 ] ];
         make_matrix_test "Valid Rectangle" true [ [ 1; 2 ]; [ 3; 4 ] ];
         make_matrix_test "Ragged Rows" false [ [ 1; 2; 3 ]; [ 4; 5 ] ];
         make_matrix_test "Zero Columns" false [ []; [] ];
         make_matrix_test "Single Row" true [ [ 1; 2; 3 ] ];
       ]

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
         make_rev_test "Multiple Elements" [ 18; 12; 9; 0; -23; -98 ] [ 18; 9; 12; -23; 0; -98 ];
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
         ("empty" >:: fun _ -> assert_raises (Failure "empty") (fun () -> list_max []));
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
           is_valid_tests;
           row_vector_tests;
           add_matrix_tests;
           multiply_matrix_tests;
         ])
