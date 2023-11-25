open OUnit2
open Solutions

let make_test name problem test_func =
  name >:: fun _ ->
  print_endline (name ^ ": solution for Problem " ^ problem);
  test_func ()

let test_last =
  make_test "test_last" "01" (fun _ ->
      assert_equal (last [ "a"; "b"; "c"; "d" ]) (Some "d");
      assert_equal (last []) None)

let test_last_two =
  make_test "test_last_two" "02" (fun _ ->
      assert_equal (last_two [ "a"; "b"; "c"; "d" ]) (Some ("c", "d"));
      assert_equal (last_two [ "a" ]) None)

let test_at =
  make_test "test_at" "03" (fun _ ->
      assert_equal (at 3 [ "a"; "b"; "c"; "d"; "e" ]) (Some "c");
      assert_equal (at 3 [ "a" ]) None)

let test_length =
  make_test "test_length" "04" (fun _ ->
      assert_equal (length [ "a"; "b"; "c" ]) 3;
      assert_equal (length []) 0)

let test_rev =
  make_test "test_rev" "05" (fun _ ->
      assert_equal (rev [ "a"; "b"; "c" ]) [ "c"; "b"; "a" ])

let test_is_palindrome =
  make_test "test_is_palindrome" "06" (fun _ ->
      assert_equal (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]) true;
      assert_equal ((not (is_palindrome [ "a"; "b" ])) = true) true)

let test_flatten =
  make_test "test_flatten" "07" (fun _ ->
      assert_equal
        (flatten
           [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])
        [ "a"; "b"; "c"; "d"; "e" ])

let test_compress =
  make_test "test_compress" "08" (fun _ ->
      assert_equal
        (compress
           [
             "a";
             "a";
             "a";
             "a";
             "b";
             "c";
             "c";
             "a";
             "a";
             "d";
             "e";
             "e";
             "e";
             "e";
           ])
        [ "a"; "b"; "c"; "a"; "d"; "e" ])

let test_pack =
  make_test "test_pack" "09" (fun _ ->
      assert_equal
        (pack
           [ "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e" ])
        [
          [ "a"; "a"; "a" ];
          [ "b" ];
          [ "c"; "c" ];
          [ "a"; "a" ];
          [ "d"; "d" ];
          [ "e"; "e"; "e" ];
        ])

let test_encode =
  make_test "test_encode" "10" (fun _ ->
      assert_equal
        (encode
           [
             "a";
             "a";
             "a";
             "a";
             "b";
             "c";
             "c";
             "a";
             "a";
             "d";
             "e";
             "e";
             "e";
             "e";
           ])
        [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ])

let test_encode_rle =
  make_test "test_encode_rle" "11" (fun _ ->
      assert_equal
        (encode_rle
           [
             "a";
             "a";
             "a";
             "a";
             "b";
             "c";
             "c";
             "a";
             "a";
             "d";
             "e";
             "e";
             "e";
             "e";
           ])
        [
          Many (4, "a");
          One "b";
          Many (2, "c");
          Many (2, "a");
          One "d";
          Many (4, "e");
        ])

let test_decode_rle =
  make_test "test_decode_rle" "12" (fun _ ->
      assert_equal
        (decode_rle
           [
             Many (4, "a");
             One "b";
             Many (2, "c");
             Many (2, "a");
             One "d";
             Many (4, "e");
           ])
        [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])

let test_encode_dir =
  make_test "test_encode_dir" "13" (fun _ ->
      assert_equal
        (encode_dir
           [
             "a";
             "a";
             "a";
             "a";
             "b";
             "c";
             "c";
             "a";
             "a";
             "d";
             "e";
             "e";
             "e";
             "e";
           ])
        [
          Many (4, "a");
          One "b";
          Many (2, "c");
          Many (2, "a");
          One "d";
          Many (4, "e");
        ])

let test_duplicate =
  make_test "test_duplicate" "14" (fun _ ->
      assert_equal
        (duplicate [ "a"; "b"; "c"; "c"; "d" ])
        [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ])

let test_replicate =
  make_test "test_replicate" "15" (fun _ ->
      assert_equal
        (replicate [ "a"; "b"; "c" ] 3)
        [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ])

let test_drop =
  make_test "test_drop" "16" (fun _ ->
      assert_equal
        (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
        [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ])

let test_split =
  make_test "test_split" "17" (fun _ ->
      assert_equal
        (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
        ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]);
      assert_equal (split [ "a"; "b"; "c"; "d" ] 5) ([ "a"; "b"; "c"; "d" ], []))

let test_slice =
  make_test "test_slice" "18" (fun _ ->
      assert_equal
        (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6)
        [ "c"; "d"; "e"; "f"; "g" ])

let test_rotate =
  make_test "test_rotate" "19" (fun _ ->
      assert_equal
        (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
        [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ];
      assert_equal
        (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2))
        [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ])

let test_remove_at =
  make_test "test_remove_at" "20" (fun _ ->
      assert_equal (remove_at 1 [ "a"; "b"; "c"; "d" ]) [ "a"; "c"; "d" ])

let suite =
  "suite"
  >::: [
         test_last;
         test_last_two;
         test_at;
         test_length;
         test_rev;
         test_is_palindrome;
         test_flatten;
         test_compress;
         test_pack;
         test_encode;
         test_encode_rle;
         test_decode_rle;
         test_encode_dir;
         test_duplicate;
         test_replicate;
         test_drop;
         test_split;
         test_slice;
         test_rotate;
         test_remove_at;
       ]

let () = run_test_tt_main suite
