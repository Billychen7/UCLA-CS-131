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
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [N Term; N Binop; N Expr];
    Expr, [N Term];
    Term, [N Num];
    Term, [N Lvalue];
    Term, [N Incrop; N Lvalue];
    Term, [N Lvalue; N Incrop];
    Term, [T"("; N Expr; T")"];
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

let gram =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

(* function that takes in a NT value and returns its alternative list *)
let productionFunc listOfRules nontermVal =
	let matchingRules = List.filter (fun rule -> (fst rule) = nontermVal) listOfRules in
	List.map (fun (first, second) -> second) matchingRules (* remove the LHS from each rule, thus returning an alternative list *)

(* converts hw1 style grammar to hw2 style grammar *)
let convert_grammar gram1 = 
	let startSymbol = (fst gram1) and
	listOfRules = (snd gram1) in
	(startSymbol, productionFunc listOfRules)


type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal



let rec parse_tree_leaves = function
  | Leaf terminalSymbol -> [terminalSymbol]
  | Node (_, childrenHead::childrenTail) -> (parse_tree_leaves childrenHead) @ (parse_node_children childrenTail)
  | _ -> []
and parse_node_children = function
  | [] -> []
  | (h::t) -> (parse_tree_leaves h) @ (parse_node_children t)






(*
parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5; Leaf 6])]))
*)

(*
(Node ("*", [Leaf 4; Leaf 5]))
*)

(* (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))    *)

(*
ptl (Node (0, [Node(1, [Leaf "a"; Leaf "b"]); Node(2, [Leaf "c"; Node(3, [Leaf "d"; Leaf "e"])]); Node(4, [Leaf "f"; Leaf "g"])]));;
*)

let xyzzdsd = 4 (* delete this later *)

(* currDerivation = startsymbol *)
(* currDerivation = [Expr] *)

let accept_all string = Some string

let accept = accept_all

let start = Expr
let prodFunc = snd gram
let frag = ["3";"+";"4";"-"]

let altList = prodFunc start (* [[N Term; N Binop; N Expr]; [N Term]] *)


let rec parse_alternative_list altList =
  match altList with
  | [] -> None (* no more valid rules - backtrack *)
  | (firstRule::otherRules) -> 
    let returnValOfParseRHS = parse_right_hand_side firstRule in
    match returnValOfParseRHS with
      | None -> parse_alternative_list otherRules (* no matches found for current rule (head) *)
      | Some x -> x

and parse_right_hand_side currRHS = 
  match currRHS with
  | [] -> accept frag (* we've found a match - call acceptor *) (* IDK *)
  | (firstSymbol::otherSymbols) -> parse_symbol firstSymbol

and parse_symbol currSymbol =
  match currSymbol with 
    | (N nonterminalSym) -> parse_alternative_list (prodFunc nonterminalSym) (* parse that symbol's alt list *)
    | (T terminalSym) ->
      match (terminalSym = (List.hd frag)) with
        | true -> accept frag
        | false -> None
          (* pass in LOL with T"1" as head *)
          (* if list is empty, return None *)








(*

let symbolName x = match x with
  N nonterminal -> nonterminal
  | _ -> failwith "Error: only nonterminal symbols should be given to this function"

let isNonterminalSymbol x = match x with
  N nonterminal -> true
  | T terminal -> false

let start = [N Expr]
let prodFunc = snd gram

let leftmostNTsym elementList =
  List.find_opt isNonterminalSymbol elementList

(*
["0"; N Expr; N Num]
*)

let applyRule currElement prodFunc =
  let alternativeList = prodFunc (symbolName currElement) in
  let firstRule = List.hd alternativeList in
  firstRule


(* returns [] if no NT symbols *)
let rec replaceLeftmostNTsym firstHalf secondHalf prodFunc =
  match secondHalf with
    | [] -> []
    | _ -> 
      let currElement = List.hd secondHalf in
      match (isNonterminalSymbol currElement) with
        | false -> replaceLeftmostNTsym (firstHalf @ [currElement]) (List.tl secondHalf) prodFunc
        | true -> firstHalf @ (applyRule currElement prodFunc) @ (List.tl secondHalf)



let start2 = [N Term]

let rec parse_rules currDerivation prodFunc =
  let leftmostNTsymbol = leftmostNTsym currDerivation in

  match (isNonterminalSymbol (List.hd currDerivation)) with
    | false -> currDerivation
    | true ->
      match leftmostNTsymbol with
        | Some symbol -> 
          let nextDerivation = replaceLeftmostNTsym [] currDerivation prodFunc in
          parse_rules nextDerivation prodFunc
        | None -> currDerivation






(*
let make_matcher gram =
  let startSymbol = (fst gram) in
*)

*)



