
#use "hw1.ml";;


let my_subset_test0 = subset [2;1;2;1;1] [1;2;3]

let my_equal_sets_test0 = equal_sets [1;2;3] [3;1;3;2]

let my_set_union_test0 = equal_sets (set_union [3;1;3;4] [1;2;3;5]) [1;2;3;4;5]

let my_set_intersection_test0 = equal_sets (set_intersection [] [1;2;3]) []

let my_set_diff_test0 = equal_sets (set_diff [1;3;5] [1;4;3;1]) [5]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 4) 1000000000 = 0

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let my_filter_reachable_test0 = filter_reachable giant_grammar = giant_grammar

let my_filter_reachable_test1 = filter_reachable (Grunt, List.tl (snd giant_grammar)) =
    (Grunt,
     [Grunt, [T "khrgh"]])

let my_filter_reachable_test2 = filter_reachable (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])
