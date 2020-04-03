
#use "hw2.ml";;

let accept_all string = Some string
type matching_parenth_nonterminals = | Curly | Comment

let matching_parenth_grammar = 
  (Curly,
    function
      | Curly -> 
        [[T"{"; T"}"];
         [T"{"; T"}"; N Comment];
         [T"{"; N Curly; T"}"];
         [N Comment; T"{"; T"}"]]
      | Comment -> 
        [[T"(*"; T"*)"];
         [T"(*"; T"*)"; N Curly];
         [T"(*"; N Comment; T"*)"];
         [N Curly; T"(*"; T"*)"]])

let fragment = ["{"; "{"; "}"; "(*"; "*)"; "{"; "}"; "}"]
let make_matcher_test = 
  ((make_matcher matching_parenth_grammar accept_all fragment) = Some [])
let make_parser_test = 
  let parser = make_parser matching_parenth_grammar fragment 
  in match parser with
    | None -> false
    | Some tree -> if parse_tree_leaves tree = fragment then true (*should return true*)
                   else false
