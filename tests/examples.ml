open Pretty_print
open Test_framework

(* Type Constructor Tests *)

(* These construct test objects for generic Pass/Fail testing *)
let test_1 = Test("Test 1: True", fun () -> true);;
let test_2 = Test("Test 2: False", fun () -> false);;

(* These construct verbose test objects that report PASS/FAIL and some additional messages *)
let test_3 = Verbose_Test("Test 3: Verbose True", fun () -> (true, "T3"));;
let test_4 = Verbose_Test("Test 4: Verbose False", fun () -> (false, "T4"));;

(* Test Builder Tests *)

(* These helper functions construct tests that 
   compare a function's result to an expected value *)
let test_5 = mk_expect_test (fun () -> 1) 1 "Test 5: Int =";;
let test_6 = mk_expect_test (fun () -> 1) 2 "Test 6: Int !=";;

let test_gt         = mk_greater_than_test (fun () -> 1) 0 false "Greater than test";;
let test_gtinc      = mk_greater_than_test (fun () -> 2) 1 true  "Greater than inclusive test";;
let test_lt         = mk_less_than_test    (fun () -> 3) 4 false "Less than test";;
let test_ltinc      = mk_less_than_test    (fun () -> 4) 4 true  "Less than inclusive test";;
let test_bounded    = mk_bounded_test      (fun () -> 5) 4 false 6 false "Bounded test";;
let test_boundedinc = mk_bounded_test      (fun () -> 6) 5 true 6 true "Bounded inclusive test";;

(* These helper functions construct verbose tests that 
	compare a function's result to an expected value, 
  reporting the comparsion between the two *)
let test_7 = mk_verbose_expect_test (fun () -> 1) 1 string_of_int "Verbose Test 7: Int =";;
let test_8 = mk_verbose_expect_test (fun () -> 1) 2 string_of_int "Verbose Test 8: Int !=";;

(* This is an example of how to use pretty printing *)
print_formatted_string "\nExamples\n\n" Blink Cyan;;

(* Test Runners *)

(* These function calls run single, isolated tests, right away *)
run_expect_test (fun () -> 3) 3 "Run Test 1: Int =";;
run_expect_test (fun () -> 3) 4 "Run Test 2: Int !=";;

(* These function calls run single, isolated, verbose tests, right away *)
run_verbose_expect_test (fun () -> 3) 3 string_of_int "Run Verbose Test 3: Int ="
;;
run_verbose_expect_test (fun () -> 3) 4 string_of_int "Run Verbose Test 4: Int !="
;;

(* These function calls run a series of tests, 
	 grouped under a name and with an overall result *)
run_test_set [ test_1; test_2; test_3; test_4; ] "Constructor Tests";;

run_test_set [ test_gt; test_gtinc; test_lt; test_ltinc; test_bounded; test_boundedinc; ] "Comparsion tests";;

run_test_set [ test_5; test_6; test_7; test_8; ] "Builder Tests";;