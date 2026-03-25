let sum_sq l = List.fold_right (fun e -> fun r -> e*e + r) l 0 
;;

let sum_sq l = List.fold_left (fun r -> fun e -> e*e + r) 0 l
;;

let sum_sq l = 
  match l with
  | [] -> 0
  | x :: l' -> x*x + sum_sq l'

let sum_sq_trec l = let rec sum_sq_rec liste res = match liste with
    | [] -> res
    | x :: l' -> sum_sq_rec l' (res + x*x)
  in sum_sq_rec l 0
;;

let inclus l1 l2 = 
  List.for_all (fun elem1 -> List.exists (fun elem2 -> (elem1 = elem2)) l2) l1
;; 

let inclus l1 l2 = List.fold_left (fun r -> fun elem -> 
    r && List.exists (fun elem2 -> (elem = elem2)) l2) true l1
;;

let inclus l1 l2 = List.fold_right (fun elem -> fun r -> 
    r && List.exists (fun elem2 -> (elem = elem2)) l2) l1 true
;;

let sup2 l1 l2 = List.for_all (fun elem1 -> 
    List.exists (fun inf -> (elem1 > inf)) l2 
    && List.exists (fun sup -> (elem1 < sup)) l2) l1
;;

let sup2 l1 l2 = 
  let liste_min l = 
    List.fold_left (fun r -> fun elem -> if r < elem then r else elem) 
      (List.hd l) l
  and liste_max l = 
    List.fold_left (fun r -> fun elem -> if r > elem then r else elem) 
      (List.hd l) l 
  in let min2 = liste_min l2 and max2 = liste_max l2 in List.for_all 
    (fun elem1 -> min2 < elem1 && elem1 < max2) l1
;;


let sum l = List.fold_right (+) l 0
;;

let pair_sup l = let testp i = (i mod 2 = 0) in let testip i = not (testp i)
  in (sum (List.filter testp l) > sum (List.filter testip l))
;;

let split l = let rec split_req l (lp, lip) = match l with
    | [] -> (lp, lip)
    | x :: l' -> let pair = (x mod 2 = 0) 
        in if pair then split_req l' (x :: lp, lip)
        else split_req l' (lp, x :: lip)
  in split_req l ([], [])
;;

let pair_sup_trec l = let split_list = split l
  in sum (fst split_list) > sum (snd split_list)
;;

