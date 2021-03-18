(*
* DS N°1
*
* Auteur : Nathan Fallet (contact@nathanfallet.me)
* https://github.com/MPSI1Thuillier/OptionInfo/blob/master/ds1/ds1.ml
*
* A éxécuter dans un terminal (ex: bash) :
* > ocaml ./ds1.ml
*
* Ceci est ma version du DS, je ne garantis pas que tout est bon, mais 
* je partage ma méthode et comment j'ai procédé.
*
* Note : les assert sont des tests effectués sur les fonctions pour
* vérifier qu'elles fonctionnent. Ils n'affichent rien dans la console,
* mais envoient une erreur si le test échoue. (Assert_failure)
* https://ocaml.org/learn/tutorials/null_pointers_asserts_and_warnings.html
*)

(*
* Exercice 1
*)

let vador n =
    if n mod 2 = 0 then
        print_int n;
        print_string " est pair";
        print_newline ()

(* 1. La fonction vador est de type int -> unit *)

(* 2. Seul la première ligne après le if est pris en compte par la condition, le reste est donc éxécuté *)

(* 3. *)
let vador n =
    print_int n;
    (* On peut simplifier encore plus la condition en changeant la string selon n mod 2 *)
    print_string (if n mod 2 = 0 then " est pair" else " est impair");
    print_newline ()

let () = vador 2
let () = vador 3

(* 4.a. *)
let estPair n = if n mod 2 = 0 then true else false

let () = assert (estPair 2)
let () = assert (not (estPair 3))

(* 4.b. *)
let estPair n =
    match n mod 2 with
    | 0 -> true
    (*
    * On ne met pas 1 mais _ ici pour match tous les autres cas,
    * sinon on a une erreur car il existe des entiers qui ne seront pas match,
    * même si n mod 2 ne retourne que 0 ou 1
    *)
    | _ -> false

let () = assert (estPair 2)
let () = assert (not (estPair 3))

(* 4.c. *)
let estPair n = n mod 2 = 0

let () = assert (estPair 2)
let () = assert (not (estPair 3))

(*
* Exercice 2
*)

(*
* 1.a. Il faudrait vérifier que n est différent de 1,
* sinon 1 est considéré comme premier comme il n'a aucun
* autre diviseur que lui même
*)

(* 1.b. *)
let estPremier n =
    let d = ref 2 in
    let response = ref true in
    while !d < n do begin
        if n mod !d = 0 then response := false;
        d := !d + 1
    end done;
    !response && n <> 1

let () = assert (not (estPremier 1))
let () = assert (estPremier 5)
let () = assert (not (estPremier 9))

(* 1.c. On effectue n-2 calculs de restes *)

(* 2.a. *)
let estPremier n =
    let d = ref 2 in
    let response = ref true in
    (* On sort de la boucle si on trouve un diviseur *)
    while !response && !d < n do begin
        if n mod !d = 0 then response := false;
        d := !d + 1
    end done;
    !response && n <> 1

let () = assert (not (estPremier 1))
let () = assert (estPremier 5)
let () = assert (not (estPremier 9))

(*
* 3.a.
* Disjonction des cas :
* Si p <= sqrt(n), c'est bon
* Si p > sqrt(n), alors n = pq donc n/q > sqrt(n) donc q/n < 1/sqrt(n) et q < sqrt(n), c'est bon
*)

(* 3.b. *)
let estPremier n =
    let d = ref 2 in
    let response = ref true in
    (* On va jusqu'à racine de n au lieu de n *)
    while !response && !d <= int_of_float (sqrt (float_of_int n)) do begin
        if n mod !d = 0 then response := false;
        d := !d + 1
    end done;
    !response && n <> 1

let () = assert (not (estPremier 1))
let () = assert (estPremier 5)
let () = assert (not (estPremier 9))

(*
* 4.a. On va démontrer la contraposé :
* Si m = 2 [6], m = 6k + 2 est divisible par 2 donc m n'est pas premier
* Si m = 3 [6], m = 6k + 3 est divisible par 3 donc m n'est pas premier
* Si m = 4 [6], m = 6k + 4 est divisible par 2 donc m n'est pas premier
* Conclusion : non (m = ±1 [6]) => non (m est premier)
* donc par contraposé, m est premier => m = ±1 [6] CQFD
*)

(*
* 4.b.
* I. Faux : par exemple, si k = 4, b = 6k + 1 = 25 n'est pas premier
* II. Vrai : Si p = 1, a + p*n > sqrt(n) donc on sort de la boucle
* III. Vrai : Car si p = 1 alors la fonction renverait true car n > 1, et on veut que ça renvoit false
*)

(* 4.c.  *)

(* 4.d. *)
let estPremier n =
    let p = ref 0 in
    let a = ref 2 in
    let b = ref 3 in
    let k = ref 1 in
    while !a + !p * n <= int_of_float(sqrt(float_of_int n)) do begin
        if (n mod !a) * (n mod !b) = 0 then
            p := 1
        else begin
            a := 6 * !k - 1;
            b := 6 * !k + 1;
            k := !k + 1
        end
    end done; !p = 0 && n > 1

let () = assert (not (estPremier 1))
let () = assert (estPremier 5)
let () = assert (not (estPremier 9))

(* 5.a. *)
let rec somme_premiers n =
    match n with (* J'ai voulu essayer sans cette ligne, mais ça ne marche pas (syntax error sur le "| 1 -> 0"), donc faut la garder *)
    | 1 -> 0
    | _ -> (if estPremier n then n else 0) + somme_premiers (n-1)

let () = assert ((somme_premiers 10) = 17)

(* 5.a. Autre manière de l'écrire *)
let rec somme_premiers n =
    match n with
    | 0 -> 0
    | n when estPremier n -> n + somme_premiers (n-1)
    | _ -> somme_premiers (n-1)

let () = assert ((somme_premiers 10) = 17)

(* 5.a En récursivité terminale *)
let somme_premiers n =
    let rec somme_premiers_aux n a =
        match n with
        | 0 -> a
        | n when estPremier n -> somme_premiers_aux (n-1) (a+n)
        | _ -> somme_premiers_aux (n-1) a
    in somme_premiers_aux n 0

let () = assert ((somme_premiers 10) = 17)

(* 5.b. *)
let rec premier_suivant n = if estPremier (n+1) then (n+1) else premier_suivant (n+1)

let () = assert ((premier_suivant 8) = 11)

(*
* Exercice 3
*)

let terrain = [|
    [|0;0;2;3;1;0|];
    [|3;3;1;1;1;3|];
    [|0;1;3;2;1;0|];
    [|0;0;1;1;1;0|];
    [|0;3;1;2;3;3|];
    [|0;0;1;1;1;0|]
|]

(* 1. *)
let ligne terrain k =
    let compte = ref 0 in
    for i = 0 to 5 do
        if terrain.(k).(i) = 2 then compte := !compte + 1
    done; !compte

let () = assert ((ligne terrain 2) = 1)

(* 2. *)
let arbre terrain (l, c) =
    (l > 0 && terrain.(l-1).(c) = 3) || (* Nord *)
    (l < 5 && terrain.(l+1).(c) = 3) || (* Sud *)
    (c < 5 && terrain.(l).(c+1) = 3) || (* Est *)
    (c > 0 && terrain.(l).(c-1) = 3)(* Ouest *)

let () = assert (arbre terrain (0, 2))
let () = assert (arbre terrain (2, 3))
let () = assert (arbre terrain (4, 3))

(* 3. *)
let verif terrain =
    let ok = ref true in
    for i = 0 to 5 do
        for j = 0 to 5 do
            if terrain.(i).(j) = 2 && not (arbre terrain (i, j)) then ok := false
        done
    done; !ok

let () = assert (verif terrain)

(*
* Exercice 4
*)

(* 1.a *)
let rec puiss1 a n =
    match n with (* Encore une fois cette ligne (match) est nécessaire *)
    | 0 -> 1
    | _ -> a * puiss1 a (n-1)

let () = assert ((puiss1 2 10) = 1024)

(* 1.b. An = n, on fait n multiplications pour obtenir a^n *)

(* 2.a *)
let rec puiss2 a n =
    match n with
    | 0 -> 1
    | 1 -> a
    | n when n mod 2 = 0 -> puiss2 (a*a) (n/2)
    | _ -> a * (puiss2 (a*a) (n/2))

let () = assert ((puiss2 2 10) = 1024)

(*
* 2.b
* B(2p+1) = 2 + B(p)
* B(p+1) = 1 + B(p)
*)

(* 2.c. *)

(*
* 3.
* On prend l'inégalité qu'on vient de démontrer, on divise par n,
* et on obtient B(n)/A(n) compris entre deux expressions qui tendent
* vers 0, donc théorème d'encadrement et on conclut.
*)

(*
* Exercice 5
*)

(* 1. *)
let next (x, y) =
    match x with
    | 0 -> (y+1, 0)
    | _ -> (x-1, y+1)

let () = assert ((next (2, 1)) = (1, 2))
let () = assert ((next (0, 4)) = (5, 0))

(* 2. *)
let rec couple n =
    match n with
    | 0 -> (0, 0)
    | _ -> next (couple (n-1))

let () = assert ((couple 7) = (2, 1))

(*
* 3. La seule option que je vois est de faire une fonction previous qui
* retourne le couple précédent, et de procéder comme la fonction couple
* mais à l'envers
*)
let previous (x, y) =
    match y with
    | 0 -> (0, x-1)
    | _ -> (x+1, y-1)

let rec indice (x, y) =
    match (x, y) with
    | (0, 0) -> 0
    | _ -> (indice (previous (x, y))) + 1

let () = assert ((indice (2, 1)) = 7)
