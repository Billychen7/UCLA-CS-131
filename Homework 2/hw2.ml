type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* #1 convert_grammar *)

(* function that takes in a NT value and returns its alternative list *)
let productionFunc listOfRules nontermVal =
	let matchingRules = List.filter (fun rule -> (fst rule) = nontermVal) listOfRules in
	List.map (fun (first, second) -> second) matchingRules (* remove the LHS from each rule, thus returning an alternative list *)

(* converts hw1 style grammar to hw2 style grammar *)
let convert_grammar gram1 = 
	let startSymbol = (fst gram1) and
	listOfRules = (snd gram1) in
	(startSymbol, productionFunc listOfRules)

(* #2 parse_tree_leaves *)

let rec parse_tree_leaves = function
  | Leaf terminalSymbol -> [terminalSymbol]
  | Node (_, childrenHead::childrenTail) -> (parse_tree_leaves childrenHead) @ (parse_node_children childrenTail)
  | _ -> []
and parse_node_children = function
  | [] -> []
  | (h::t) -> (parse_tree_leaves h) @ (parse_node_children t)

(* #3 make_matcher *)

let rec parse_alternative_list prodFunc startSymbol altList accept frag =
  match altList with
  | [] -> None (* we've exhausted all the rules and didn't find a match *)
  | (firstRule::otherRules) ->
    let resultOfParseRule = parse_rule prodFunc firstRule accept frag in
    match resultOfParseRule with
    | None -> parse_alternative_list prodFunc startSymbol otherRules accept frag (* try the next rule *)
    | Some x -> Some x (* return whatever the acceptor returned *)

and parse_rule prodFunc currRule accept frag =
  match currRule with
  | [] -> accept frag (* we've matched up all the symbols of the rule - call acceptor *)
  | (firstSymbol::otherSymbols) ->
    match firstSymbol with
    | (N nonterminalSym) ->
      let curriedAcceptor = parse_rule prodFunc otherSymbols accept in (* pass the matcher with the rest of the rules as the acceptor *)
      parse_alternative_list prodFunc nonterminalSym (prodFunc nonterminalSym) curriedAcceptor frag
    | (T terminalSym) ->
      match frag with
      | [] -> None (* found a symbol with no symbols left in the fragment to match it, so backtrack *)
      | (fragHead::fragTail) ->
        match (terminalSym = fragHead) with
        | true -> parse_rule prodFunc otherSymbols accept fragTail (* found a match, now check rest of frag *)
        | false -> None (* symbols didn't match - backtrack *)

let make_matcher gram =
  let startSymbol = (fst gram)
  and prodFunc = (snd gram) in
  let altList = (prodFunc startSymbol) in
  (fun accept frag -> parse_alternative_list prodFunc startSymbol altList accept frag)

(* #4 make_parser *)

(* if the frag is empty, that means it has been parsed entirely *)
let parse_tree_acceptor frag tree =
  match frag with
  | [] -> Some tree
  | _ -> None

(* note: the tree is represented by: Node(startSymbol, treeChildren) *)
(* treeChildren is a list of parse_tree elements, specifically the children of the startSymbol Node *)
let rec parse_alternative_list_tree prodFunc startSymbol altList accept frag treeChildren =
  match altList with
  | [] -> None (* we've exhausted all the rules and didn't find a parse tree *)
  | (firstRule::otherRules) ->
    let resultOfParseRuleTree = parse_rule_tree prodFunc startSymbol firstRule accept frag treeChildren in
    match resultOfParseRuleTree with
    | None -> parse_alternative_list_tree prodFunc startSymbol otherRules accept frag treeChildren (* try the next rule *)
    | Some x -> Some x

and parse_rule_tree prodFunc startSymbol currRule accept frag treeChildren =
  match currRule with
  | [] -> accept frag (Node(startSymbol, treeChildren)) (* form the tree and call the acceptor *)
  | (firstSymbol::otherSymbols) ->
    match firstSymbol with
    | (N nonterminalSym) ->
      (* curriedAcceptor recursively calls parse_rule_tree with the rest of the rule and appends the tree argument to treeChildren *)
      let curriedAcceptor frag2 tree2 = 
        parse_rule_tree prodFunc startSymbol otherSymbols accept frag2 (treeChildren @ [tree2]) in
      parse_alternative_list_tree prodFunc nonterminalSym (prodFunc nonterminalSym) curriedAcceptor frag []
    | (T terminalSym) ->
      match frag with
      | [] -> None (* found a symbol with no symbols left in the fragment to match it, so backtrack *)
      | (fragHead::fragTail) ->
        match (terminalSym = fragHead) with
        | true -> parse_rule_tree prodFunc startSymbol otherSymbols accept fragTail (treeChildren @ [Leaf terminalSym])
        (* for true: append the current terminalSymbol to treeChildren (as a Leaf) and recursive call with tail *)
        | false -> None (* symbols didn't match - backtrack *)

let make_parser gram =
  let startSymbol = (fst gram)
  and prodFunc = (snd gram) in
  let altList = (prodFunc startSymbol) in
  (fun frag -> parse_alternative_list_tree prodFunc startSymbol altList parse_tree_acceptor frag [])
