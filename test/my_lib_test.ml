open! Core

let%expect_test _ =
  My_lib.print_hi ();
  [%expect {| hi |}]
;;
