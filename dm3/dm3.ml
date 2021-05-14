(*
* Exercice 1
* Distance de Hamming
*)

let hamming_array t1 t2 =
	if Array.length t1 <> Array.length t2 then
		failwith "t1 et t2 n'ont pas la même longueur !";
	let c = ref 0 in
	for k = 0 to (Array.length t1) - 1 do
		if t1.(k) <> t2.(k) then
			c := !c + 1
	done;
	!c;;

let hamming_list l1 l2 =
	(* c est l'accumulateur *)
	let rec hamming_list_rec l1 l2 c =
		match l1, l2 with
		| [], [] -> c
		| [], _ | _, [] ->
			failwith "l1 et l2 n'ont pas la même longueur !"
		| h1 :: t1, h2 :: t2 when h1 = h2 ->
			hamming_list_rec t1 t2 c
		| h1 :: t1, h2 :: t2 ->
			hamming_list_rec t1 t2 (c+1)
	in hamming_list_rec l1 l2 0;;

hamming_array [|1; 0; 1; 0; 0|] [|0; 1; 1; 1; 0|];;
hamming_list [1; 0; 1; 0; 0] [0; 1; 1; 1; 0];;

(*
* Exercice 2
* Compression RLE
*)

let compresser l =
	(*
	* e le dernier élément traité
	* c un compteur temporaire
	* et r le résultat
	*)
	let rec compresser_rec l e c r =
		match l with
		| [] when c = 0 -> List.rev r
		| [] ->
			compresser_rec [] e 0 (e :: c :: r)
		| h :: t when h <> e && c <> 0 ->
			compresser_rec t h 1 (e :: c :: r)
		| h :: t ->
			compresser_rec t h (c + 1) r
	in compresser_rec l 0 0 [];;

compresser [];;
compresser [1; 1; 1; 1; 2; 2; 2; 2; 2; 1; 1; 1; 4];;

let decompresser l =
	let rec decompresser_rec l e c r =
		match l with
		| l when c <> 0 ->
			decompresser_rec l e (c-1) (e :: r)
		| h1 :: h2 :: t ->
			decompresser_rec t h2 h1 r
		| h :: [] ->
			failwith "Liste invalide !"
		| [] -> List.rev r
	in decompresser_rec l 0 0 [];;

decompresser [];;
decompresser [4; 1; 5; 2; 3; 1; 1; 4];;

(*
* Exercice 3
* join et split
*)

let joindre s l =
	let rec joindre_rec s l r =
		match l with
		| [] -> r
		| h :: [] -> r ^ h
		| h :: t -> joindre_rec s t (r ^ h ^ s)
	in joindre_rec s l "";;

joindre "**" ["ab"; "cdef"; "ghi"];;

(*
* C'est apas joli parce qu'on peut pas faire
* de match sur une string, mais ça fonctionne
*)
let scinder str sep =
	let len = String.length str in
	let l = ref [] in
	let lastSep = ref 0 in
	for k = 0 to len-1 do
		let c = str.[k] in
		if c = sep then begin
			l := (String.sub str !lastSep (k - !lastSep)) :: !l;
			lastSep := k + 1
		end
	done;
	l := (String.sub str !lastSep (len - !lastSep)) :: !l;
	List.rev !l;;

scinder "abc;def;ghij" ';';;

(*
* Exercice 4
* Escalier
*)

let escalier l =
	let rec escalier_rec l s =
		match l with
		| [] -> s
		| h :: h2 :: t when h2 > h ->
			escalier_rec t (s+h2)
		| h :: t ->
			escalier_rec t (s+h)
	in escalier_rec l 0;;

escalier [5; -3; -6; 4; 3; -2];;
