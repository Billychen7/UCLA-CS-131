(*
Supply at least one test case for each of the above functions in the style shown in the sample test cases below. 
When testing the function F call the test cases my_F_test0, my_F_test1, etc. 
For example, for subset your first test case should be called my_subset_test0. 
Your test cases should exercise all the above functions, even though the sample test cases do not.
*)

(* subset test cases *)
let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [4;5;6]
let my_subset_test2 = not (subset [4;5;6] [])
let my_subset_test3 = subset [4;5;6] [4;5;6]
let my_subset_test4 = subset [6;5;4] [4;5;6]
let my_subset_test5 = subset [6;5;4;5;6] [4;5;6]
let my_subset_test6 = subset [5;4;5] [4;5;6]
let my_subset_test7 = not (subset [4;5;6] [4;5;5])
let my_subset_test8 = subset ["a";"b"] ["a";"b";"c"]
let my_subset_test8 = not (subset ["a";"b";"c"] ["a";"c"])

(* equal_sets test cases *)
let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [1;2;3] [])
let my_equal_sets_test2 = not (equal_sets [] [1;2;3])
let my_equal_sets_test3 = equal_sets [1;2;3] [1;2;3]
let my_equal_sets_test4 = equal_sets [1;2;3] [3;2;1]
let my_equal_sets_test5 = equal_sets [1;2;3] [1;2;3;2;1;2;3]
let my_equal_sets_test6 = not (equal_sets [1;2;2] [1;2;3])
let my_equal_sets_test7 = equal_sets ["a";"b";"c"] ["c";"a";"b"]
let my_equal_sets_test8 = not (equal_sets ["a";"b";"c"] ["c";"b"])

(* set_union test cases *)
let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [] [4;5;6]) [4;5;6]
let my_set_union_test2 = equal_sets (set_union [4;5;6] []) [4;5;6]
let my_set_union_test3 = equal_sets (set_union [4;5;6] [4;5;6]) [4;5;6]
let my_set_union_test4 = equal_sets (set_union [4;5;6] [6;5;4]) [4;5;6]
let my_set_union_test5 = equal_sets (set_union [4;5;6] [4;5;4]) [4;5;6]
let my_set_union_test6 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let my_set_union_test7 = equal_sets (set_union ["a";"b";"c";"d"] ["c";"d";"e";"f"]) ["a";"b";"c";"d";"e";"f"]

(* set_intersection test cases *)
let my_set_intersection_test0 = equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [] [4;5;6]) []
let my_set_intersection_test2 = equal_sets (set_intersection [4;5;6] []) []
let my_set_intersection_test3 = equal_sets (set_intersection [4;5;6] [4;5;6]) [4;5;6]
let my_set_intersection_test4 = equal_sets (set_intersection [4;5;6] [6;5;4]) [4;5;6]
let my_set_intersection_test5 = equal_sets (set_intersection [4;5;6] [7;6;5;4;3;2]) [4;5;6]
let my_set_intersection_test6 = equal_sets (set_intersection ["a";"b";"c";"d"] ["c";"d";"e";"f"]) ["c";"d"]

(* set_diff test cases *)
let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] []) [1;2;3]
let my_set_diff_test2 = equal_sets (set_diff [] [1;2;3]) []
let my_set_diff_test3 = equal_sets (set_diff [1;2;3] [1;2;3]) []
let my_set_diff_test4 = equal_sets (set_diff [1;2;3] [3;2;1]) []
let my_set_diff_test5 = equal_sets (set_diff [1;2;3] [1;1;2;3;2;3;1]) []
let my_set_diff_test6 = equal_sets (set_diff [1;2;3] [0;1;2;3;4]) []
let my_set_diff_test7 = equal_sets (set_diff [1;2;3;4;5] [2;4]) [1;3;5]
let my_set_diff_test8 = equal_sets (set_diff [1;2;3] [1;2;2]) [3]
let my_set_diff_test9 = equal_sets (set_diff ["a";"b";"c"] ["a"]) ["b";"c"]

(* computed_fixed_point test cases *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> 2) 5 = 2
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. x) 5. = infinity
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> ~- x) 0 = 0






