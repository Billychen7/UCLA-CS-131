(* subset: returns true if a is a subset of b *)
let rec subset a b = match a with
	[] -> true
	| _ -> 
		if (List.mem (List.hd a) b) then
			subset (List.tl a) b
		else
			false

(* equal_sets: returns true if a and b are equal sets *)
let equal_sets a b =
	(subset a b) && (subset b a)

(* set_union: returns a list representing a union b *)
(* we make a the union by adding all elements in b to a that aren't already in a *)
let rec set_union a b = match b with
	[] -> a (* if b is empty, then a is the union *)
	| _ ->
		if (List.mem (List.hd b) a) then (* if b's head is contained in a *)
			set_union a (List.tl b) (* recursively call function with tail of b *)
		else (* if b's head is NOT contained in a *)
			set_union (List.cons (List.hd b) a) (List.tl b) (* add b's head to a, then  make recursive call *)

(* set_intersection: returns a list representing a intersect b *)
let set_intersection a b =
	List.filter (fun x -> (List.mem x b)) a

(* set_diff: returns a list representing a - b *)
let set_diff a b =
	List.filter (fun x -> not (List.mem x b)) a

(* computed_fixed_point: computes the fixed point for f with respect to x, assuming eq is the equality predicate for f's domain *)
let rec computed_fixed_point eq f x =
	if (eq x (f x)) then
		x
	else
		computed_fixed_point eq f (f x)
