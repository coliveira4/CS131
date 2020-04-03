type ('a, 'b) symbol = N of 'a | T of 'b

let rec mem a b = 
    match b with
    | [] -> false
    | hd::tl when hd = a -> true
    | hd::tl -> mem a tl
    ;;
     
let rec subset a b = 
    match a with
    | [] -> true
    | hd::tl when (mem hd b) -> subset tl b
    | hd::tl -> false
    ;;    
    
let equal_sets a b =
    subset a b && subset b a
    ;;
    
let rec set_union a b = 
    match a with
    | [] -> b
    | hd::tl when (mem hd b) -> set_union tl b
    | hd::tl -> set_union tl (b @ [hd])
    ;;    
      
let rec set_intersection a b = 
    match a with
    | [] -> []
    | hd::tl when (mem hd b) -> ([hd] @ (set_intersection tl b))
    | hd::tl -> set_intersection tl b
    ;;   
    
let rec set_diff a b = 
    match a with
    | [] -> []
    | hd::tl when not (mem hd b) -> ([hd] @ (set_diff tl b))
    | hd::tl -> set_diff tl b
    ;; 
    
let rec computed_fixed_point eq f x = 
 if eq x (f x)
 then x
 else computed_fixed_point eq f (f x)
    ;; 
  
let rec remove_stuff l =
    match l with
    | [] -> []
    | hd::tl -> match hd with 
                | N(x) -> x :: remove_stuff tl
                | T(y) -> remove_stuff tl
;;

let rec match_rules rules x =
    match rules with
    | [] -> []
    | hd::tl when (x = (fst hd)) -> (snd hd) @ (match_rules tl x)
    | hd::tl -> match_rules tl x
;;
 
let rec filter_work to_look_at rules looked_at =
    match to_look_at with
    | [] -> looked_at
    | hd::tl when not (mem hd looked_at) -> filter_work (remove_stuff ((match_rules rules hd)) @ tl) rules (hd :: looked_at)
    | hd::tl -> filter_work tl rules looked_at  
;;  

let rec final_filter l rules result=
    match rules with
    | [] -> result
    | hd::tl when (mem (fst hd) l) -> final_filter l tl (result @ [hd])
    | hd::tl -> final_filter l tl result
;;

let rec filter_reachable g =
    (fst g,
    final_filter (filter_work [fst g] (snd g) []) (snd g) [])
;;
