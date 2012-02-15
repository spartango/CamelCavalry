open Pretty_print

(* Tests types, verbose tests give a message alongside success value *)
type test = Verbose_Test of string * (unit -> bool * string) | Test of string * (unit -> bool)

(* Makes a test that expects a particular value from the target fucntion *)
let mk_expect_test (f : unit -> 'a) (expected : 'a) (name : string) : test = 
    let t_test = fun () -> (f ()) = expected in
    Test(name, t_test)

(* Makes a test that checks if an output value is within a particular range (inclusive) *)
let mk_bounded_test (f   : unit -> 'a) 
                    (lower_bound : 'a) 
                    (upper_inc : bool) 
                    (upper_bound : 'a) 
                    (lower_inc : bool) 
                    (name    : string) : test = 
    let t_test = 
        fun () -> 
            (if upper_inc then ((f ()) >= lower_bound) 
                          else (((f ()) > lower_bound))) 
            &&
            (if lower_inc then ((f ()) <= upper_bound)
                          else ((f ()) < upper_bound)) in
    Test(name, t_test)

let mk_vebose_compare_test (f : unit -> 'a) 
                           (comparator : 'a -> 'a -> bool) 
                           (target: 'a) 
                           (to_string : 'a -> string) 
                           (name : string) : test = 
    let t_test  = 
        fun () -> 
            let result  = (f ())                     in 
            let pass    = (comparator result target) in
            let message = 
                if pass 
                then "Got expected result -> "^(format_string (to_string result) Bright Green) 
                else "Result doesn't match expected -> "^(format_string (to_string result) Bright Red)
                     ^" vs "^(format_string (to_string target) Bright Green)
            in (pass, message)
    in
    Verbose_Test(name, t_test)

let mk_compare_test (f : unit -> 'a) (comparator : 'a -> 'a -> bool) (target: 'a) (name : string) : test = 
    let t_test = 
        fun () -> 
            (comparator (f ()) target)
    in
    Test(name, t_test)

(* Makes a test that checks if an output value is greater than or equal to a certain value (inclusive) *)
let mk_greater_than_test (f : unit -> 'a) (floor : 'a) (inclusive : bool) (name : string) : test = 
    let op = (if inclusive then (>=) else (>)) in
    mk_compare_test f op floor name

(* Makes a test that checks if an output value is less than or equal to a certain value (inclusive) *)
let mk_less_than_test (f : unit -> 'a) (ceiling : 'a) (inclusive : bool) (name : string) : test = 
    let op = (if inclusive then (<=) else (<)) in
    mk_compare_test f op ceiling name

(* Makes a test that expects a particular value from f, and prints differences
 * it fails to match that value *)
let mk_verbose_expect_test (f : unit -> 'a) (expected : 'a) (to_string : 'a -> string) (name : string) : test = 
    mk_vebose_compare_test f (=) expected to_string name

(* Runs a single test *)
let run_test  (t_test : test) : (bool * string) = 
    match t_test with 
        | Test(name, exec) ->
            let result = (exec ()) in 
            (result,  
            (if result then (format_string "[  PASSED  ] " Bright Green) 
                       else (format_string "[  FAILED  ] " Bright Red))
                ^name^"\n")
        | Verbose_Test(name, exec) -> 
            let (result, message) = (exec ()) in 
            (result,  
            (if result then (format_string "[  PASSED  ] " Bright Green) 
                       else (format_string "[  FAILED  ] " Bright Red))
                ^name^": "^message^"\n")
                 
            
(* Runs a set of tests together under a set name, grouping their output *)
let run_test_set (tests : test list) (set_name : string) : unit = 
    let _ = print_string ((format_string "\n[==========] " Bright Cyan)^"Running "^set_name^"\n")  in
    let rec run_tests_h ( tests : test list ) ( pass : bool ) : bool = 
        match tests with 
            | []             -> pass
            | t_test :: rest -> 
                let (result, message) = (run_test t_test) in 
                let _ = print_string message in 
                (run_tests_h rest (pass && result))
    in
    let pass = run_tests_h tests true in 
    print_string ((format_string "[==========] " Bright Cyan)^"Tests "
                   ^(if pass then (format_string "Passed" Bright Green) else (format_string "Failed" Bright Red))^"\n\n")

(* Runs a bunch of tests under a generic name *)
let run_tests (tests : test list) : unit = 
    (run_test_set tests "Tests") 
            
(* Runs a single expect test (making it from the params *)
let run_expect_test  (f : unit -> 'a) (expected : 'a) (name : string) =
    run_test ( mk_expect_test f expected name )

(* Makes and runs a single verbose expect test *)
let run_verbose_expect_test (f : unit -> 'a) (expected : 'a) (to_string : 'a -> string) (name : string) = 
    run_test ( mk_verbose_expect_test f expected to_string name )

let test_stub = Test("Implemented", (fun () -> false)  )
;;
