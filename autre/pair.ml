(*
* Différentes manière de checker si un nombre est pair
*)

(* Classique *)
let pair n = n mod 2 = 0

let _ = assert (pair 12)
let _ = assert (not (pair 13))

(* Utilisation d'un opérateur binaire *)
let pair n = n land 1 = 0

let _ = assert (pair 12)
let _ = assert (not (pair 13))

(* Utilisation de la division d'un entier *)
let pair n = (n / 2) * 2 = n

let _ = assert (pair 12)
let _ = assert (not (pair 13))

(* Même principe avec un opérateur binaire *)
let pair n = (n lsr 1) lsl 1 = n

let _ = assert (pair 12)
let _ = assert (not (pair 13))

(* Manière récursive *)
let rec pair n =
    match n with
    | 0 -> true
    | _ -> not (pair (n-1))

let _ = assert (pair 12)
let _ = assert (not (pair 13))

(* Manière récursive terminale *)
let pair n =
    let rec pair_rec b n = 
        match n with
        | 0 -> b
        | _ -> pair_rec (not b) (n-1)
    in pair_rec true n

let _ = assert (pair 12)
let _ = assert (not (pair 13))

(* Conversion en string *)
let pair n =
    let str = string_of_int n in
    let lastChr = str.[(String.length str) - 1] in
    lastChr = '0' || lastChr = '2' || lastChr = '4' || lastChr = '6' || lastChr = '8'

let _ = assert (pair 12)
let _ = assert (not (pair 13))

(*
* D'autres idées ? Faites vos popositions avec une Pull Request !
*)
