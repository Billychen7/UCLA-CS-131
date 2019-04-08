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

type nonterminals =
  | Sentence | Noun | NP | Verb | Adjective | Adverb

let rules =
  [Sentence, [N NP; N Verb; N NP];
   Sentence, [N NP; N Verb];
   NP, [N Adjective; N Noun];
   NP, [N Noun];
   Noun, [T"apple"];
   Noun, [T"Paul"; T"Eggert"];
   Verb, [T"love"];
   Adjective, [T"crispy"];
   Adverb, [T"happily"]]

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

(* function that given a set of rules and a starting point, add all rules with that starting
point as their LHS *)
let rec allRulesWithGivenStartingPoint start rules resultList = match rules with
	[] -> resultList
	| _ -> 
		match (start = (fst (List.hd rules))) with
		true -> allRulesWithGivenStartingPoint start (List.tl rules) (List.cons (List.hd rules) resultList)
		| false -> allRulesWithGivenStartingPoint start (List.tl rules) resultList
(*
	let x = allRulesWithGivenStartingPoint Sentence rules [];;
	returns: [(Sentence, [N NP; N Verb]); (Sentence, [N NP; N Verb; N NP])]
*)

let isNonterminalSymbol x = match x with
	N nonterminal -> true
	| T terminal -> false

(* function that concatenates a bunch of lists *)
let rec concatenateLists matchingRules outputList = match matchingRules with
	[] -> outputList
	| _ -> 
		let currList = (snd (List.hd matchingRules)) in
		concatenateLists (List.tl matchingRules) (outputList @ currList)

let filterListForOnlyNTsymbols inputList =
	List.filter isNonterminalSymbol inputList

(* function that takes in startPoint, and returns [startPoint; NT symbols in LHS] *)
(* takes in Sentence and returns [N Sentence; N NP; N Verb; N NP; N Verb; N NP] *)
let allDirectNTsymbolsGivenStart start listOfRules =
	let matchingRules = allRulesWithGivenStartingPoint start listOfRules [] in
	(* matchingRules = [(Sentence, [N NP; N Verb]); (Sentence, [N NP; N Verb; N NP])] *)
	let allSymbolsInTheRHS = concatenateLists matchingRules [] in
	(* now filter the list *)
	let filteredSymbolsInTheRHS = filterListForOnlyNTsymbols allSymbolsInTheRHS in
	(* filteredSymbolsInTheRHS = [N NP; N Verb; N NP; N Verb; N NP] *)
	List.cons (N start) filteredSymbolsInTheRHS (* add the start point *)

let symbolName x = match x with
	N nonterminal -> nonterminal
	(* | T terminal -> terminal *)

(* now we need a function that given an input list such as [N Sentence; N NP; N Verb], *)
(* it calls allDirectNTsymbolsGivenStart for each entry in the list, and then appends their lists *)
let rec findAllReachableNTforGivenList inputList listOfRules outputList = match inputList with
	[] -> outputList
	| _ ->
		let currSymbols = allDirectNTsymbolsGivenStart (symbolName (List.hd inputList)) listOfRules in
		findAllReachableNTforGivenList (List.tl inputList) listOfRules (outputList @ currSymbols)

let helperFunc inputList listOfRules =
	findAllReachableNTforGivenList inputList listOfRules []
	
let reachableNonterminals inputList listOfRules =
	(* findAllReachableNTforGivenList inputList listOfRules [];; *)
	computed_fixed_point equal_sets (fun list -> (findAllReachableNTforGivenList list listOfRules [])) inputList

(*
(* takes in Sentence, [N NP; N Verb; N NP] *)
(* returns true if (N Sentence is in reachable) *)
let isReachable rule reachableNTs = 
	let lhs = (N (fst rule)) in
	List.mem lhs reachableNTs
*)

let filter_reachable g =
	let startSymbol = (N (fst g)) in
	let grammarRules = (snd g) in
	let reachableNTs = reachableNonterminals [startSymbol] grammarRules in
	(  (fst g)   , List.filter (fun rule -> 
					let lhs = (N (fst rule)) in
					List.mem lhs reachableNTs) grammarRules )


	(* forogt to include starting point *)


