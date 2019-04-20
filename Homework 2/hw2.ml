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

type nonterms =
  | PHRASE | NOUN | VERB

let kimmogram = 
  (PHRASE,
   function
     | PHRASE -> [[N NOUN; N VERB]]
     | NOUN -> [[T "mary"];[T "mark"]]
     | VERB -> [[T "eats"];[T "drinks"]])

let kimaccept = function 
  | "pizza"::t -> Some ("pizza"::t) 
  | _ -> None

let kimmofrag = ["mark"; "eats"; "pizza"]

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









let start = Expr
let prodFunc = snd gram
let frag = ["9"; "+"; "$"; "1"; "+"]

let accept_all string = Some string

let altList = prodFunc start (* [[N Term; N Binop; N Expr]; [N Term]] *)


let rec parse_alternative_list prodFunc startSymbol altList accept frag =
  match altList with
  | [] -> None (* we've exhausted all the rules and didn't find a match *)
  | (firstRule::otherRules) ->
    let resultOfParseRule = parse_rule prodFunc firstRule accept frag in
    match resultOfParseRule with
    | None -> parse_alternative_list prodFunc startSymbol otherRules accept frag (* try the next rule *)
    | Some x -> Some x

and parse_rule prodFunc currRule accept frag =
  match currRule with
  | [] -> accept frag (* we've matched up all the symbols of the rule *)
  | (firstSymbol::otherSymbols) ->
    match firstSymbol with
    | (N nonterminalSym) ->
      let curriedAcceptor = parse_rule prodFunc otherSymbols accept in
      parse_alternative_list prodFunc nonterminalSym (prodFunc nonterminalSym) curriedAcceptor frag
    | (T terminalSym) ->
      match frag with
      | [] -> None (* found a symbol with no symbols left in the fragment to match it, so backtrack *)
      | (fragHead::fragTail) ->
        match (terminalSym = fragHead) with
        | true -> parse_rule prodFunc otherSymbols accept fragTail (* found a match, now check rest of frag *)
        | false -> None (* backtrack *)

let make_matcher gram =
  let startSymbol = (fst gram)
  and prodFunc = (snd gram) in
  let altList = (prodFunc startSymbol) in
  (fun accept frag -> parse_alternative_list prodFunc startSymbol altList accept frag)


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



(* currDerivation = startsymbol *)
(* currDerivation = [Expr] *)

(*
let accept_all string = Some string
*)
(*
let accept = accept_all
*)

(*
let start = Expr
let prodFunc = snd gram
let frag = ["9"; "+"; "$"; "1"; "+"]

let altList = prodFunc start (* [[N Term; N Binop; N Expr]; [N Term]] *)
*)








(*

let rec parse_alternative_list prodFunc accept frag altList =
  match altList with
  | [] -> None (* no more valid rules - backtrack *)
  | (firstRule::otherRules) -> 
    let returnValOfParseRHS = parse_right_hand_side prodFunc accept frag altList firstRule in
    match returnValOfParseRHS with
      | None -> parse_alternative_list prodFunc accept frag otherRules (* no matches found for current rule (head) *)
      | Some x -> Some x

and parse_right_hand_side prodFunc accept frag altList currRHS = 
  match currRHS with
  | [] -> accept (List.tl frag) (* we've found a match - call acceptor *) (* IDK *)
  | (firstSymbol::otherSymbols) -> parse_symbol prodFunc accept frag altList currRHS firstSymbol

and parse_symbol prodFunc accept frag altList currRHS currSymbol =
  match currSymbol with 
    | (N nonterminalSym) -> parse_alternative_list prodFunc accept frag (prodFunc nonterminalSym) (* parse that symbol's alt list *)
    | (T terminalSym) ->
      match (terminalSym = (List.hd frag)) with
        | true -> Some "I DONT UNDERSTAND WHAT TO RETURN"
            (* we reach this point when we've derived "mark" but we need "mark eats" *)
            (* we have to do something with the tail of the fragment *)
          accept (List.tl frag) (* pass the suffix to the acceptor *)
        | false -> 
          match altList with
            | [] -> None
            | (firstRule::otherRules) -> parse_alternative_list prodFunc accept frag otherRules
          (* basically try the next rule *)
          (* pass in LOL with T"1" as head *)
          (* if list is empty, return None *)



let make_matcher gram =
  let startSymbol = (fst gram)
  and prodFunc = (snd gram) in
  let altList = (prodFunc startSymbol) in
  (fun accept frag -> parse_alternative_list prodFunc accept frag altList)


*)

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



