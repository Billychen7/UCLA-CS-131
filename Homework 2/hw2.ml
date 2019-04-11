type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*
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
   Adjective, [T"crispy"]]
  *)

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

(* function that takes in every rule with a certain starting point and returns that starting point's alternative list *)
(* given: [(Sentence, [N NP; N Verb; N NP]); (Sentence, [N NP; N Verb])] *)
(* returns: [[N NP; N Verb; N NP]; [N NP; N Verb]] *)
let rec alternativeList = function
	| [] -> []
	| h::t -> 
		let currRHS = (snd h) in
		currRHS :: (alternativeList t)

(* function that takes in a NT value and returns its alternative list *)
let productionFunc listOfRules nontermVal =
	let matchingRules = List.filter (fun rule -> (fst rule) = nontermVal) listOfRules in
	alternativeList matchingRules

(* converts hw1 style grammar to hw2 style grammar *)
let convert_grammar gram1 = 
	let startSymbol = (fst gram1) and
	listOfRules = (snd gram1) in
	(startSymbol, productionFunc listOfRules)


