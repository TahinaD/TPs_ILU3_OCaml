let rec doublons l =
  match l with
  | [] -> []
  | x :: tl -> if List.mem x tl then let d = doublons tl 
        in if List.mem x d then d else x :: d else doublons tl
;;

let rec get a0 l = 
  match l with
  | [] -> failwith "aucun couple trouvé"
  | (a, b) :: tl -> if a = a0 then b else get a0 tl
;;
    
let rec put a0 b0 l =
  match l with
  | [] -> [(a0, b0)]
  | (a, b) :: tl -> if a = a0 then (a, b0) :: tl else (a, b) :: put a0 b0 tl
;;


module ENV_LIST : tENV_LIST = struct
  type ('x, 'v) env = ('x * 'v) list
  let empty = []
  let get = get
  let put = put
end

module ENV_FUN : tENV_FUN = struct
  type ('x, 'v) env = 'x -> 'v
  let empty _ = failwith "valeur inexistante"
  let get x env = env x
  let put x y env = fun v -> if v = x then y else env v
end


let regle_if (expr : expr) = 
  match expr with
  | If(c, BConst true, BConst false) -> c
  | If(c1, BConst true, c2) -> Call(Or, c1, c2)
  | If(c1, c2, BConst false) -> Call(And, c1, c2)
  | _ -> expr
;;

let rec apply regle (e : expr) =
  match e with
  | IConst(i) -> regle e
  | BConst(bo) -> regle e
  | Var(s) -> regle e
  | If(a,b,c) -> regle (If(apply regle a, apply regle b, apply regle c))
  | Let(s,x,y) -> regle (Let(s, apply regle x, apply regle y))
  | Call(p, q, r) -> regle (Call(p, apply regle q, apply regle r))
;;

module EVAL : tEVAL = functor (E : tENV) -> struct
  type value = Vint of int | Vbool of bool
  let eval_op b v1 v2 = 
    match b, v1, v2 with
    | And, Vbool(b1), Vbool(b2) -> Vbool(b1 && b2)
    | Or, Vbool(b1), Vbool(b2) -> Vbool(b1 || b2)
    | Add, Vint(i1), Vint(i2) -> Vint(i1 + i2)
    | Leq, Vint(i1), Vint(i2) -> Vbool(i1 <= i2)
    | _ -> failwith "incompatibles"
  let rec eval env expr = 
    match expr with
    | IConst(i) -> Vint(i)
    | BConst(b) -> Vbool(b)
    | Var(s) -> E.get s env
    | If(_, _, _) -> eval env (apply regle_if expr)
    | Let(s, e1, e2) -> eval (E.put s (eval env e1) env) e2
    | Call(b, t1, t2) -> eval_op b (eval env t1) (eval env t2)
end


module TYPECHECK : tTYPECHECK = functor (E : tENV) -> struct
  let typeof_op bop = 
    match bop with
    | And -> (Bool, Bool, Bool)
    | Or -> (Bool, Bool, Bool)
    | Add -> (Int, Int, Int)
    | Leq -> (Int, Int, Bool)
  let rec typeof env expr =
    match expr with
    | IConst(i) -> Int
    | BConst(b) -> Bool
    | Var(s) -> E.get s env
    | If(cond, e1, e2) -> if typeof env cond = Bool 
        then let type1 = typeof env e1 and type2 = typeof env e2
          in if ((type1 = Int || type1 = Bool) && (type2 = Int || type2 = Bool))
          then Bool else failwith "expression mal typée"
        else failwith "expression mal typée"
    | Let(var, vale, e) -> let typevale = typeof env vale 
        in let envvar = E.put var typevale env in let typevar = E.get var envvar
        in if typevar = typevale then typevale else failwith "expression mal typée"
    | Call(bop, e1, e2) -> let bontyp = typeof_op bop 
        in let thrd (a,b,c) = c 
        in if bontyp = (typeof env e1, typeof env e2, thrd bontyp)
        then thrd bontyp else failwith "expression mal typée"
end


;;
(* etc. *)
