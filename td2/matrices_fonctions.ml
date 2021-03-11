(*
* Exercice 1
*)

let somme matrice =
	let sum = ref 0 in
	for i = 0 to Array.length(matrice) - 1 do
		for j = 0 to Array.length(matrice.(i)) - 1 do
			sum := !sum + matrice.(i).(j)
		done
	done;
	!sum;;

let nbsup matrice seuil =
	let count = ref 0 in
	for i = 0 to Array.length(matrice) - 1 do
		for j = 0 to Array.length(matrice.(i)) - 1 do
			count := !count + if matrice.(i).(j) > seuil then 1 else 0
		done
	done;
	!count;;

let transpose matrice =
	let line = Array.length matrice in
	let col = Array.length matrice.(0) in
	let newM = Array.make_matrix col line matrice.(0).(0) in
	for i = 0 to col-1 do
		for j = 0 to line-1 do
			newM.(i).(j) <- matrice.(j).(i)
		done
	done;
	newM;;
	
(*
* Exercice 2
*)

let carte = [|
	[|-1; 7; 6; -1; 4; 6|];
	[|7; -1; -1; -1; -1; 10|];
	[|6; -1; -1; 5; 4; 5|];
	[|-1; -1; 5; -1; -1; 6|];
	[|4; -1; 4; -1; -1; -1|];
	[|6; 10; 10; 6; -1; -1|]
|];;

let voisins carte ville =
	let result = Array.make 6 false in
	for k = 0 to 5 do
		result.(k) <- carte.(ville).(k) <> -1
	done;
	result;;

let longueur carte villes =
	let len = ref 0 in
	for k = 0 to Array.length(villes) - 2 do
		let d = carte.(villes.(k)).(villes.(k+1)) in
		len := if d <> -1 && !len <> -1 then !len + d else -1
	done;
	!len;;
	
(* TODO: version récursive de la fonction longueur *)

(*
* Exercice 3
*)

let rec puissance a n = if n > 0 then a * puissance a (n-1) else 1;;

let rec factorielle n = if n > 0 then n * factorielle (n-1) else 1;;

(*
* Exercice 4
*)

let exemple1 f = f(1) + 1;;

let exemple2 a b = a + b;;

let exemple3 a f = f (a+1) + 1;;

(*
* Exercice 5
*)

(*
* 1. int -> int
* 2. int -> int -> bool
*)

(*
* Exercice 6
*)

let implique_logique a b = not a || b;;

let implique_filtrage a b =
	match a with
	| true -> b
	| false -> true;;

(*
* Série de tests
*)

assert ((somme [| [|1; 2; 3|]; [|4; 5; 6|] |]) = 21);;

assert ((nbsup [| [|1; 2; 3|]; [|4; 5; 6|] |] 3) = 3);;

assert ((transpose [| [|1; 2; 3|]; [|4; 5; 6|] |]) = [| [|1; 4|]; [|2; 5|]; [|3; 6|] |]);;

assert ((voisins carte 0) = [|false;true;true;false;true;true|]);;

assert ((longueur carte [|0; 1; 5|]) = 17);;
assert ((longueur carte [|0; 1; 2|]) = -1);;

assert ((puissance 2 10) = 1024);;

assert ((factorielle 8) = 40320);;

assert (implique_logique true true);;
assert (not (implique_logique true false));;
assert (implique_logique false true);;
assert (implique_logique false false);;

assert (implique_filtrage true true);;
assert (not (implique_filtrage true false));;
assert (implique_filtrage false true);;
assert (implique_filtrage false false);;
