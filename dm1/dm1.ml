(*
* DM N°1
*
* Auteur : Nathan Fallet (contact@nathanfallet.me)
* https://github.com/MPSI1Thuillier/OptionInfo/blob/master/dm1/dm1.ml
*
* A éxécuter dans un terminal (ex: bash) :
* > ocaml ./dm1.ml
*
* La série de tests à la fin du fichier s'éxécute,
* et affiche une erreur si quelque chose ne fonctionne
* pas correctement. Donc si rien ne s'affiche, c'est
* que tout fonctionne correctement, à priori.
*)

(*
* Exercice 1 - Fonctions sur les tableaux
*)

let concatener t1 t2 =
    let taille1 = Array.length t1 in
    let taille = taille1 + Array.length t2 in
    let resultat = Array.make taille 0 in
    for element = 0 to taille-1 do
        resultat.(element) <- if element < taille1 then t1.(element) else t2.(element-taille1)
    done;
    resultat;;

let sous_tableau t i j =
    let taille = max (j - i + 1) 0 in
    let resultat = Array.make taille 0 in
    for element = 0 to taille-1 do
        resultat.(element) <- t.(element + i)
    done;
    resultat;;

let appliquer f t =
    let taille = Array.length t in
    for i = 0 to taille-1 do
        t.(i) <- f t.(i)
    done;
    t;;

(*
* Exercice 2 - Permutations
*)

let identite n =
    let resultat = Array.make n 0 in
    for i = 1 to n-1 do
        resultat.(i) <- i
    done;
    resultat;;

let est_permutation n t =
    let test = Array.make n 0 in
    let taille = Array.length t in
    for i = 0 to taille-1 do
        test.(t.(i)) <- test.(t.(i)) + 1
    done;
    test = Array.make n 1;;

(* Cette fonction est_permutation a une complexité O(n) *)

let composee f1 f2 =
    let taille = Array.length f1 in
    let resultat = Array.make taille 0 in
    for i = 0 to taille-1 do
        resultat.(i) <- f2.(f1.(i))
    done;
    resultat;;

let reciproque f =
    let taille = Array.length f in
    let resultat = Array.make taille 0 in
    for i = 0 to taille-1 do
        resultat.(f.(i)) <- i
    done;
    resultat;;

let rec puissance f p =
    match p with
    | 0 -> identite (Array.length f)
    | 1 -> f
    | _ -> composee f (puissance f (p-1));;

(*
* Note : On pourrait se passer du cas p = 1, car on aurait la composée
* de f avec l'identité, ce qui reviendrait au même, avec une étape en plus
*)

let ordre f =
    let id = identite (Array.length f) in
    let c = ref f in
    let o = ref 1 in
    while !c <> id do
        c := composee !c f;
        o := !o + 1
    done;
    !o;;

(*
* Vérification de tous les résultats
* Une simple série de tests qui check que tout fonctionne
* Une erreur sera lancée si un des tests échoue (Assert_failure)
*
* Voir https://ocaml.org/learn/tutorials/null_pointers_asserts_and_warnings.html
*)

assert((concatener [|3; 2; 5|] [|-1; 2; 6; 3|]) = [|3; 2; 5; -1; 2; 6; 3|]);;

assert((sous_tableau [|7; 2; -1; 6; 3; 1|] 2 4) = [|-1; 6; 3|]);;
assert((sous_tableau [|7; 2; -1; 6; 3; 1|] 2 1) = [||]);;

assert((appliquer (function x -> 3*x) [|1; 2; 3; 4|] = [|3; 6; 9; 12|]));;

assert(identite 5 = [|0; 1; 2; 3; 4|]);;

assert(est_permutation 5 [|3; 0; 2; 4; 1|]);;
assert(not (est_permutation 5 [|3; 0; 2; 4; 0|]));;

assert((composee [|3; 0; 2; 1|] [|0; 2; 3; 1|]) = [|1; 0; 3; 2|]);;

assert((reciproque [|3; 1; 0; 4; 2|]) = [|2; 1; 4; 0; 3|]);;
assert((composee [|3; 1; 0; 4; 2|] (reciproque [|3; 1; 0; 4; 2|])) = [|0; 1; 2; 3; 4|]);;

assert((puissance [|3; 1; 0; 4; 2|] 0) = [|0; 1; 2; 3; 4|]);;
assert((puissance [|3; 1; 0; 4; 2|] 2) = [|4; 1; 3; 2; 0|]);;

assert((ordre [|3; 1; 0; 4; 2|]) = 4);;
