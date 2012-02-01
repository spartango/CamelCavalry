open Pretty_print
open Test_framework

(* Type Constructor Tests *)
let test_1 = Test("Test 1: True", fun () -> true);;
let test_2 = Test("Test 2: False", fun () -> false);;

let test_3 = Verbose_Test("Test 3: Verbose True", fun () -> (true, "T3"));;
let test_4 = Verbose_Test("Test 4: Verbose False", fun () -> (false, "T4"));;

(* Test Builder Tests *)
let test_5 = mk_expect_test (fun () -> 1) 1 "Test 5: Int =";;
let test_6 = mk_expect_test (fun () -> 1) 2 "Test 6: Int !=";;

let test_7 = mk_verbose_expect_test (fun () -> 1) 1 string_of_int "Verbose Test 7: Int =";;
let test_8 = mk_verbose_expect_test (fun () -> 1) 2 string_of_int "Verbose Test 8: Int !=";;

print_formatted_string "\nExamples\n\n" Blink Cyan;;

(* Test Runners *)
run_expect_test (fun () -> 3) 3 "Run Test 1: Int =";;
run_expect_test (fun () -> 3) 4 "Run Test 2: Int !=";;

run_verbose_expect_test (fun () -> 3) 3 string_of_int "Run Verbose Test 3: Int ="
;;
run_verbose_expect_test (fun () -> 3) 4 string_of_int "Run Verbose Test 4: Int !="
;;

run_test_set [ test_1; test_2; test_3; test_4; ] "Constructor Tests";;

run_test_set [ test_5; test_6; test_7; test_8; ] "Builder Tests";;