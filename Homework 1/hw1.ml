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

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* functions that only returns the rules with a given starting symbol as their LHS *)
let allRulesWithGivenStartingPoint start rules =
	List.filter (fun rule -> (fst rule = start)) rules

let isNonterminalSymbol x = match x with
	N nonterminal -> true
	| T terminal -> false

let filterListForOnlyNTsymbols inputList =
	List.filter isNonterminalSymbol inputList

let rec concatenateLists matchingRules outputList = match matchingRules with
	[] -> outputList
	| _ -> 
		let currList = (snd (List.hd matchingRules)) in
		concatenateLists (List.tl matchingRules) (outputList @ currList)

(* function that takes in startPoint and rules, and returns [startPoint; NT symbols in RHS for that point] *)
(* for example, takes in Sentence and returns [N Sentence; N NP; N Verb; N NP; N Verb; N NP] *)
let allDirectNTsymbolsGivenStart start listOfRules =
	let matchingRules = allRulesWithGivenStartingPoint start listOfRules in (* get only the rules starting with 'start' *)
	let allSymbolsInTheRHS = concatenateLists matchingRules [] in (* get every symbol in the RHS of those rules *)
	let filteredSymbolsInTheRHS = filterListForOnlyNTsymbols allSymbolsInTheRHS in (* filter out any T symbols *)
	List.cons (N start) filteredSymbolsInTheRHS (* add the start point to the list *)

let symbolName x = match x with
	N nonterminal -> nonterminal
	| _ -> failwith "Error: only nonterminal symbols should be given to this function"

(* given an input list such as [N Sentence; N NP; N Verb] *)
(* this function calls allDirectNTsymbolsGivenStart for each entry in the list, and then concatenates their lists *)
let rec findAllReachableNTforGivenList inputList listOfRules outputList = match inputList with
	[] -> outputList
	| _ ->
		let currSymbols = allDirectNTsymbolsGivenStart (symbolName (List.hd inputList)) listOfRules in
		findAllReachableNTforGivenList (List.tl inputList) listOfRules (outputList @ currSymbols)

(* recursively finds all reachable NT symbols until they all have been found *)
let reachableNonterminals inputList listOfRules =
	computed_fixed_point equal_sets (fun list -> (findAllReachableNTforGivenList list listOfRules [])) inputList

let filterOutUnreachable reachableNTs grammarRules =
	List.filter (fun rule -> let lhs = (N (fst rule)) in List.mem lhs reachableNTs) grammarRules

let filter_reachable g =
	let startSymbol = (fst g) in
	let grammarRules = (snd g) in
	let reachableNTs = reachableNonterminals [N startSymbol] grammarRules in
	(startSymbol, filterOutUnreachable reachableNTs grammarRules)
