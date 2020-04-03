type ('a, 'b) symbol = N of 'a | T of 'b
open List
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x 
;;
let match_empty_matcher accept frag = accept frag;;
let match_nothing_matcher accept frag = None;;
let match_empty_parser accept frag_dev frag = accept frag_dev frag;;
let match_nothing_parser accept frag_dev frag = None;;
let accept frag_dev suffix = Some (frag_dev, suffix);;
(*returns rules as a funtion w ith rules grouped, but maintain order*)
let rec convert_helper rules x =
	match rules with
	| [] -> []
	| hd::tl -> if fst hd = x then (snd hd)::(convert_helper tl x) else convert_helper tl x
;;
(*converts hw1 style gram to hw2 style gram*)
let convert_grammar gram1 = 
	match gram1 with
	| (hd,tl) -> (hd, convert_helper tl )(*hd is the keyword, which we want to keep and tl is the rule list*)
 ;;
(*traverses tree L to R and returns a list of leaves encountered*)
(*tcant use empty keyword with definition given? so Just put in list, send to helper function*)
let rec parse_tree_leaves_helper tree = 
    match tree with
  	| [] -> []
  	| hd::tl -> 
  		match hd with 
    	| Leaf leaf -> leaf::(parse_tree_leaves_helper tl)
    	| Node (nonterminal, subtree) -> (parse_tree_leaves_helper subtree) @ (parse_tree_leaves_helper tl);;
let parse_tree_leaves tree = parse_tree_leaves_helper [tree];;

(*make_matcher_helpers*)
let rec matcher matcher_prod beginning = function 
  | [] -> match_nothing_matcher 
  | hd::tl -> fun accept frag ->
      let head_matcher = check_rules matcher_prod hd accept frag 
      and tail_matcher = matcher matcher_prod beginning tl 
      in match head_matcher with
        | None -> tail_matcher accept frag
        | _ -> head_matcher (*match, return acceptor output*)
 and check_rules matcher_prod = function 
  | [] -> match_empty_matcher (*return acceptor output*)
  | (N nonterminal)::tl -> let rules = matcher_prod nonterminal (*head is nonterminal, go down this path*)
    in fun accept frag -> let new_accept = check_rules matcher_prod tl accept (* choose new suffix and check *)
      in matcher matcher_prod nonterminal rules new_accept frag
  | (T terminal)::tl -> (fun accept -> function
      | [] -> None
      | f_hd::f_tl -> if f_hd = terminal then check_rules matcher_prod tl accept f_tl else None);;
 (*returns a matcher for gram*)
let make_matcher gram = 
  let beginning = fst gram in let matcher_prod = snd gram in let rules = matcher_prod beginning in
  fun accept frag -> matcher matcher_prod beginning rules accept frag;; 

(*parsing time!*)
(*parse_helpers, basically same as above but slightly modified as shown*)
let rec matcher_parser matcher_prod beginning = function 
  | [] -> match_nothing_parser 
  | hd::tl -> fun accept frag_dev frag -> let head_matcher = check_rules_parser matcher_prod hd accept (frag_dev @ [(beginning, hd)]) frag (* when we get a match, use frag_dev instead*)
      and tail_matcher = matcher_parser matcher_prod beginning tl (* check the rest for matches *)
      in match head_matcher with
        | None -> tail_matcher accept frag_dev frag
        | _ -> head_matcher (*match, return acceptor output*)
and check_rules_parser matcher_prod = function 
  | [] -> match_empty_parser (*return acceptor output*)
  | (N nonterminal)::tl -> let rules = matcher_prod nonterminal (*head is nonterminal, go down this path*)
    in fun accept frag_dev frag -> let new_accept = check_rules_parser matcher_prod tl accept (*change from matcher function to include frag_dev*)
      in matcher_parser matcher_prod nonterminal rules new_accept frag_dev frag
  | (T terminal)::tl -> (fun accept frag_dev -> function
      | [] -> None (*list fully checked with no matches*)
      | f_hd::f_tl -> if f_hd = terminal then check_rules_parser matcher_prod tl accept frag_dev f_tl else None);; (*fragment match found*)

let rec construct_tree = function
  | (nonterminal, rule)::frag_dev_tail -> let paths = search_nodes frag_dev_tail rule in (*incompmete pattern matching?*)
  			match paths with
           | (dev_tail, path_tail) -> let node = Node (nonterminal, path_tail) in dev_tail, node
  and search_nodes dev_tail = function 
  | [] -> dev_tail, []
  | (N nonterminal)::tl -> (let path_tail = construct_tree dev_tail (*nonterminal reached, keep going*)
    in match path_tail with
      | (dev_tail_x, curr_paths) -> let other_rules_to_leaves = search_nodes dev_tail_x tl 
        in match other_rules_to_leaves with
          | (dev_tail_y, other_rules_paths) -> let all_paths = (curr_paths::other_rules_paths) in dev_tail_y, all_paths)
  | (T terminal)::tl -> (let subtrees = search_nodes dev_tail tl in match subtrees with
      | (dev_tail_x, additional_paths) -> let leaf = ((Leaf terminal)::additional_paths) in dev_tail_x, leaf);;

let make_parser gram = 
  let beginning = fst gram in
  let matcher_prod = snd gram in
  let rules = matcher_prod beginning in
  fun frag -> let frag_dev = matcher_parser matcher_prod beginning rules accept [] frag
    in match frag_dev with
      | None -> None
      | Some (prefix, suffix) -> if suffix == [] then 
                                 let parse_tree = construct_tree prefix
                                 in match parse_tree with
                                 | (_, tree) -> Some tree 
                                 else None;;