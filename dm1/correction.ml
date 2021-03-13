(* Exercice 1 *)

(* 1 *)

let concatener t1 t2 =
  let n1 = Array.length t1 and n2 = Array.length t2 in
  let t = Array.make (n1+n2) t1.(0) in
  for i = 0 to n1-1 do t.(i) <- t1.(i) done;
  for j = 0 to n2-1 do t.(n1+j) <- t2.(j) done;
  t;;

concatener [|3; 2; 5|] [|8; 2; -6; 3|];;
concatener [||] [|3; 5|];;  (* Erreur à cause du t1.(0). *)

let concatener t1 t2 =
  let n1 = Array.length t1 and n2 = Array.length t2 in
  if n1 = 0 then t2 else
    let t = Array.make (n1+n2) t1.(0) in
    for i = 0 to n1-1 do t.(i) <- t1.(i) done;
    for j = 0 to n2-1 do t.(n1+j) <- t2.(j) done;
    t;;

concatener [|3; 2; 5|] [|8; 2; -6; 3|];;
concatener [||] [|3; 5|];;
concatener [|3; 2; 5|] [||];;
concatener [||] [||];;


(* 2 *)

let sous_tableau t i j =
  let res = Array.make (j-i+1) t.(0) in
  for k = i to j do res.(k-i) <- t.(k) done;
  res;;

sous_tableau [|7; 2; -1; 6; 3; 1|] 2 4;;
sous_tableau [|7; 2; -1; 6; 3; 1|] 3 1;; (* Erreur. *)
sous_tableau [|7; 2; -1; 6; 3; 1|] 2 10;; (* Erreur. *)
sous_tableau [||] 2 4;; (* Erreur. *)

let sous_tableau t i j =
  let n = Array.length t in
  if n = 0 || i >= n || i > j then [||] else
    let q = min j (n-1) in
    let res = Array.make (q-i+1) t.(0) in
    for k = i to q do res.(k-i) <- t.(k) done;
    res;;

sous_tableau [|7; 2; -1; 6; 3; 1|] 2 4;;
sous_tableau [|7; 2; -1; 6; 3; 1|] 3 1;;
sous_tableau [|7; 2; -1; 6; 3; 1|] 2 10;;
sous_tableau [|7; 2; -1; 6; 3; 1|] 30 40;;
sous_tableau [||] 2 4;;

(* 3 *)

let appliquer f t =
  let n = Array.length t in
  if n = 0 then [||] else 
    let res = Array.make n t.(0) in
    for i = 0 to n-1 do res.(i) <- f t.(i) done;
    res;;

appliquer (function x -> 3*x) [|1; 2; 3; 4|];;
appliquer (function x -> 3*x) [||];;


(* Exercice 2 *)

(* 1 *)

let identite n =
  let t = Array.make n 0 in
  for i = 1 to n-1 do t.(i) <- i done;
  t;;

identite 5;;

(* 2 *)

(* Une solution en O(n^2) : *)

let est_permutation n t =
  if Array.length t <> n then false else
    let res = ref true and i = ref 0 in
    while !i < n && !res do
      for j = !i+1 to n-1 do
        if t.(!i) = t.(j) then res := false;
      done;
      i := !i + 1
    done;
    !res;;

est_permutation 5 [|3; 0; 2; 4; 1|];;
est_permutation 5 [|3; 0; 2; 4; 0|];;
est_permutation 4 [|3; 0; 2; 4; 0|];;

(* Une solution en O(n*ln(n)) : *)

let est_permutation n t =
  if Array.length t <> n then false else
    let tt = Array.copy t in  (* Pour ne pas modifier t. *)
      Array.sort compare tt;  (* Tri (complexité O(n*ln(n))). *)
      tt = identite n;;

est_permutation 5 [|3; 0; 2; 4; 1|];;
est_permutation 5 [|3; 0; 2; 4; 0|];;
est_permutation 4 [|3; 0; 2; 4; 0|];;

(* Une solution en O(n) : *)

let est_permutation n t =
  if Array.length t <> n then false else
    let occurrences = Array.make n 0 and i = ref 0 and res = ref true in
    while !i < n && !res do
      occurrences.(t.(!i)) <- occurrences.(t.(!i)) + 1;
      if occurrences.(t.(!i)) > 1 then res := false;
      i := !i + 1
    done;
    !res;;

est_permutation 5 [|3; 0; 2; 4; 1|];;
est_permutation 5 [|3; 0; 2; 4; 0|];;
est_permutation 4 [|3; 0; 2; 4; 0|];;

let est_permutation n t =
  if Array.length t <> n then false else
    let occurrences = Array.make n 0 in
    for i = 0 to n-1 do occurrences.(t.(i)) <- occurrences.(t.(i)) + 1 done;
    occurrences = Array.make n 1;;

est_permutation 5 [|3; 0; 2; 4; 1|];;
est_permutation 5 [|3; 0; 2; 4; 0|];;
est_permutation 4 [|3; 0; 2; 4; 0|];;

(* 3 *)

let composee t1 t2 =
  let n = Array.length t1 in
  let t = Array.make n 0 in
  for i = 0 to n-1 do t.(i) <- t2.(t1.(i)) done;
  t;;

composee [|3; 0; 2; 1|] [|0; 2; 3; 1|];;

(* 4 *)

let reciproque t =
  let n = Array.length t in
  let tt = Array.make n 0 in
  for i = 0 to n-1 do tt.(t.(i)) <- i done;
  tt;;

reciproque [|3; 1; 0; 4; 2|];;

(* 5 *)

(* Version itérative *)

let puissance t p =
  let res = ref (identite (Array.length t)) in
  for i = 1 to p do res := composee !res t done;
  !res;;

puissance [|3; 1; 0; 4; 2|] 0;;
puissance [|3; 1; 0; 4; 2|] 1;;
puissance [|3; 1; 0; 4; 2|] 2;;
puissance [|3; 1; 0; 4; 2|] 3;;
puissance [|3; 1; 0; 4; 2|] 4;;

(* Version récursive *)

let rec puissance t = function
  | 0 -> identite (Array.length t)
  | p -> composee t (puissance t (p-1));;

puissance [|3; 1; 0; 4; 2|] 0;;
puissance [|3; 1; 0; 4; 2|] 1;;
puissance [|3; 1; 0; 4; 2|] 2;;
puissance [|3; 1; 0; 4; 2|] 3;;
puissance [|3; 1; 0; 4; 2|] 4;;

(* 6 *)

let ordre t =
  let p = ref 1 and tp = ref t and id = identite (Array.length t) in
  while !tp <> id do 
    tp := composee !tp t;
    p := !p + 1;
  done;
  !p;;

ordre [|3; 1; 0; 4; 2|];;
ordre [|1; 3; 5; 7; 9; 0; 2; 4; 6; 8|];;
ordre [|1; 0; 3; 4; 2; 6; 7; 8; 9; 5; 11; 12; 13; 14; 15; 16; 10|];; (* 210 *)
ordre (identite 5);;


