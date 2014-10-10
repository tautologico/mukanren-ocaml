
type var = int

(* terms that carry a value of type 'a *)
type 'a term = 
  | Var of var 
  | Pair of 'a term * 'a term 
  | List of 'a term list 
  | Value of 'a 

type 'a subst = (var * 'a term) list

let walk u s = 
  if List.mem_assoc u s then 
    List.assoc u s
  else u 

let unify u v s = 
  match u, v with
  | Var v1, Var v2 when v1 = v2 -> s
  | Var v1, _ -> ext_s v1 v s
  | _, Var v2 -> ext_s v2 u s
  | Pair (t11, t12), Pair (t21, t22) -> 0
  | List l1, List l2 -> 0
  | Value v1, Value v2 when v1 = v2 -> s 
  | _ -> 0
