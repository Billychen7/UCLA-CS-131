(* sample English grammar *)
type english_nonterminals =
  | Sentence | NP | Noun | Verb | Adjective

let production_function = function
	| Sentence -> 
		[[N NP; N Verb; N NP]; 
		 [N NP; N Verb]]
	| NP ->
		[[N Adjective; N Noun];
		 [N Noun]]
	| Noun ->
		[[T "OCaml"];
		 [T "Michael"; T "Scott"]]
	| Verb ->
		[[T "loves"];
		 [T "despises"]]
	| Adjective ->
		[[T "happy"];
		 [T "distraught"];
		 [T "beautiful"]]

let english_grammar = Sentence, production_function

let accept_non_empty_suffix = function
	| [] -> None
	| x -> Some x

let sample_matcher_frag = ["distraught"; "Michael"; "Scott"; "despises"; "OCaml"]

let sample_parser_frag = ["happy"; "Michael"; "Scott"; "loves"; "beautiful"; "OCaml"]

let make_matcher_test = 
	((make_matcher english_grammar accept_non_empty_suffix sample_matcher_frag)
		= Some ["OCaml"])

let make_parser_test = 
	match make_parser english_grammar sample_matcher_frag with
	| Some tree -> parse_tree_leaves tree = sample_matcher_frag
	| _ -> false
