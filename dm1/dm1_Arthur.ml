let concat a b =
  let aux i =
    if i < Array.length a then
      a.(i)
    else
      b.(i - Array.length a)
  in
  Array.init (Array.length a + Array.length b) aux

let sub_array a x y =
  let b = min x y and c = max x y in
  Array.init (c - b + 1) (fun i -> a.(b+i))

let apply f a = Array.init (Array.length a) (fun i -> f a.(i))

let identity x = Array.init x (fun i -> i)

let is_permut a =
  let id = identity (Array.length a) in
  Array.for_all (fun i -> Array.mem i a) id
(* Dans le meilleur des cas, 0 est absent de a, le premier résultat étant faux, Array.for_all s'arrête à la première itération, complexité O(1) *)
(* Dans le pire des cas, tout les éléments sont présents, Array.for_all s'arrête à l'itération n, complexité O(n) *)

let compose a b = Array.init (Array.length a) (fun i -> b.(a.(i)))

let reciprocal a =
  let b = Array.init (Array.length a) (fun i -> i) in
  Array.iter (fun i -> b.(a.(i)) <- i) a; b

let rec power a x =
  match x with
  | 0 -> identity (Array.length a)
  | y -> compose a (power a (y-1))

let order a =
  let id = identity (Array.length a) in
  let rec aux l x =
    if (power l x) = id then
      x
    else
      aux l (x+1)
  in
  aux a