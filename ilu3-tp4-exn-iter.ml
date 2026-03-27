type prop = Case_Occupee of elem | Pince_Vide | And of prop*prop | No of prop 
;;

let contient elem = Case_Occupee elem 
let pince_est_vide = Pince_Vide 
let andp p1 p2 = And (p1,p2)
let notp p = No p
let some_prop = andp (contient Objet) (pince_est_vide)
;;

let it_prop fCase_Occupee fPince_Vide fAnd fNo prop = 
  let rec trait p = match p with
    | Case_Occupee(elem) -> fCase_Occupee elem
    | Pince_Vide -> fPince_Vide
    | And(p1, p2) -> fAnd p1 p2 (trait p1) (trait p2)
    | No(p) -> fNo p (trait p)
  in trait prop
;; 

type etat = Etat of plateau * (int * int) * etat_pince * (int * int) 
;;

let mk_etat plat pos_rob e_pince dir_rob = Etat(plat, pos_rob, e_pince, dir_rob)
;;

let get_plateau etat = match etat with
  | Etat(plat,_,_,_) -> plat
    
let get_position etat = match etat with
  | Etat(_,pos_rob,_,_) -> pos_rob
    
let get_pince etat = match etat with
  | Etat(_,_,e_pince,_) -> e_pince
    
let get_direction etat = match etat with
  | Etat(_,_,_,dir_rob) -> dir_rob
;;


let eval etat prop = 
  it_prop (fun elem -> elem = (get_plateau etat) (get_position etat))
    ((get_pince etat) = Vide)
    (fun p1 p2 trait_p1 trait_p2 -> trait_p1 && trait_p2) 
    (fun p trait_p -> not trait_p) prop
;;


let prendreCase elem = match elem with
  | Objet -> Rien
  | _ -> failwith "pas d'objet à prendre"
           
let prendreA plateau position = let contenu = plateau position
  in let valeur = prendreCase contenu in fun case -> if case = position 
    then valeur else plateau case
                
let etatPrendre etat = if eval etat (notp pince_est_vide) then failwith "pince pleine"
  else let nouv_plateau = prendreA (get_plateau etat) (get_position etat)
    in mk_etat nouv_plateau (get_position etat) Pleine (get_direction etat)
;;


let poserCase elem = match elem with
  | Rien -> Objet
  | _ -> failwith "case déjà pleine"
           
let poserA plateau position = let contenu = plateau position
  in let valeur = poserCase contenu in fun case -> if case = position 
    then valeur else plateau case

let etatPoser etat = if eval etat pince_est_vide then failwith "pince vide"
  else let nouv_plateau = poserA (get_plateau etat) (get_position etat)
    in mk_etat nouv_plateau (get_position etat) Vide (get_direction etat)
;;


let etatTourner etat = let nouv_dir (x,y) = (-y, x)
  in mk_etat (get_plateau etat) (get_position etat)
    (get_pince etat) (nouv_dir (get_direction etat))
    
let etatAvancer etat = 
  let prochaine_case = (fst(get_position etat) + fst(get_direction etat), 
                        snd(get_position etat) + snd(get_direction etat))
  and plateau = get_plateau etat
  in let contenu_proch_case = plateau prochaine_case
  in match contenu_proch_case with
  | Obstacle -> failwith "avancee impossible"
  | _ -> mk_etat plateau prochaine_case (get_pince etat) (get_direction etat)
;;


type commande = Prendre | Poser | Tourner | Avancer
              | IfProp of commande * prop 
              | ListeCommande of commande list
;;

let prendre = Prendre
let poser = Poser
let tourner = Tourner
let avancer = Avancer
let if_prop com p = IfProp(com, p)
let seq list_com = ListeCommande list_com
;;

let rec executer etat commande = match commande with
  | Prendre -> etatPrendre etat
  | Poser -> etatPoser etat
  | Tourner -> etatTourner etat
  | Avancer -> etatAvancer etat
  | IfProp(com, p) -> if eval etat p then executer etat com
      else failwith "execution impossible"
  | ListeCommande(l) -> match l with
    | [] -> etat
    | x :: l' -> executer (executer etat x) (seq l') 

let rec executer2 etat commande = match commande with
  | Prendre -> etatPrendre etat
  | Poser -> etatPoser etat
  | Tourner -> etatTourner etat
  | Avancer -> etatAvancer etat
  | IfProp(com, p) -> if eval etat p then executer etat com
      else failwith "execution impossible"
  | ListeCommande(l) -> List.fold_left executer2 etat l
;;


let rec first_occ pred liste = match liste with
  | [] -> raise Impossible
  | x :: l' -> if pred x then x else first_occ pred l' 

let first_occ_opt pred liste = try Some(first_occ pred liste) with 
  | Impossible -> None
;;


