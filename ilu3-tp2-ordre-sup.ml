let hd l =
  match l with
  | [] -> failwith "liste vide"
  | x :: l' -> x
;;

let tl l =
  match l with
  | [] -> failwith "liste vide"
  | x :: l' -> l'
;;



let rec tete m =
  match m with
  | [] -> []
  | l :: m' -> hd l :: tete m'
;;

let rec reste m =
  match m with
  | [] -> []
  | l :: m' -> tl l :: reste m'
;;

let rec trans m =
  match m with
  | [] -> []
  | [] :: _ -> []
  | m' -> tete m' :: trans (reste m')


(*('a -> 'b) -> 'a list -> 'b list*)
let rec map f l =
  match l with
  | [] -> []
  | x :: l' -> f x :: map f l'
;;

let tete2 m = map hd m
;;
let reste2 m = map tl m
;;

let rec ligzero n = if n < 0 then failwith "longueur invalide" else
    match n with
    | 0 -> []
    | _ -> 0 :: ligzero (n-1)
;;

let zero n = if n < 0 then failwith "longueur invalide" else
    (*let colzero elem = elem :: (ligzero (n-1))
     in (map colzero) (ligzero n)*)
    let l0 = ligzero n in map (fun _ -> l0) l0
;;


let rec unite n = if n < 0 then failwith "dimension invalide" else
    match n with
    | 0 -> []
    | 1 -> [[1]]
    | _ -> (*let aj0m m = let aj0l l = l @ [0] in map aj0l m 
            in (aj0m (unite (n-1))) @ [(ligzero (n-1)) @ [1]]*)
        let l1 = 1 :: ligzero(n-1) in l1 :: (map (fun l -> 0 :: l) (unite (n-1)))
;;


let rec map2 f l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | [], x::l -> failwith "taille 1 < taille 2"
  | x::l, [] -> failwith "taille 1 > taille 2"
  | x1::l1', x2::l2' -> f x1 x2 :: map2 f l1' l2'
;;

let somlig l1 l2 = map2 (+) l1 l2
;;

let add m1 m2 = map2 somlig m1 m2
;;



let rec prodligcol l c =
  match l, c with
  | [], [] -> 0
  | xl::l', xc::c' -> (xl * xc) + prodligcol l' c'
  | _ -> failwith "erreur" 
;;

let prodligtmat l m = let prodl cm = prodligcol l cm in map prodl m
;;

let prod m1 m2 = let prodlm lm = prodligtmat lm (trans m2) in map prodlm m1
;; 



let create f n = if n < 0 then failwith "longueur invalide" else 
    let rec liste k = if k = -1 then [] 
      else f (n-k) :: liste (k-1) in liste (n-1)
;;

(*let create f n = if n = 1 then [f 1] else create f (n-1) @ [f n]*)

let couples n = let lig len = create (fun i -> i) len
  in map (fun i -> map (fun j -> (i,j)) (lig n)) (lig n)
;; 

let zero2 n = let l0 num = create (fun j -> 0) num 
  and ln = create (fun i -> n) n in map l0 ln
;; 

let unite2 n = let tabi i = create (fun k -> if k = i then 1 else 0) n 
  in map tabi (create (fun j -> j) n)
;;