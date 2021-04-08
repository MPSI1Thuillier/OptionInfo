(*
* Exercice 1
*)

let index element list =
	let rec index_rec i element list =
		match list with
		| [] -> failwith "L'élément n'est pas dans la liste"
		| h :: t when h = element -> i
		| h :: t -> index_rec (i+1) element t
	in index_rec 0 element list;;
	
index 3 [1;2;3;4;5];;

index 6 [1;2;3;4;5];;

(*
* Exercice 2
*)
let maximum list = List.fold_left max (List.hd list) (List.tl list);;

maximum [1;2;3;4;5;-2];;

(*
* Exercice 3
*)
let supprime index list =
	let rec supprime_rec index list1 list2 =
		match list1 with
		| [] -> List.rev list2
		| h :: t when index = 0 -> supprime_rec (index-1) t list2
		| h :: t -> supprime_rec (index-1) t (h :: list2)
	in supprime_rec index list [];;
	
supprime 2 [1;2;3;4;5];;

let supprime_elt element list =
	let rec supprime_rec list1 list2 =
		match list1 with
		| [] -> List.rev list2
		| h :: t when h = element -> supprime_rec t list2
		| h :: t -> supprime_rec t (h :: list2)
	in supprime_rec list [];;

supprime_elt 2 [1;2;3;2;3;4;5];;

let supprime_doublons list =
	let rec contient list element =
		match list with
		| [] -> false
		| h :: t -> h = element || contient t element
	in let rec supprime_rec list1 list2 =
		match list1 with
		| [] -> List.rev list2
		| h :: t when contient list2 h -> supprime_rec t list2
		| h :: t -> supprime_rec t (h :: list2)
	in supprime_rec list [];;
	
supprime_doublons [1;2;3;4;1;1;2;5;3];;

(*
* Exercice 4
*)

let aplatir biglist =
	let rec aplatir_rec biglist result =
		match biglist with
		| [] -> result
		| h :: t -> aplatir_rec t (result @ h)
	in aplatir_rec biglist [];;
	
aplatir [[1;2];[3;4;5];[6;7];[];[8;9;10]];;

(*
* Exercice 5
*)

let inf_lex (a, b) (c, d) =
	a < c || (a = c && b <= d);;
	
inf_lex (-1, 2) (1, -5);;
inf_lex (1, 2) (1, 1);;

let rec inf_lex_list list1 list2 =
	let hd = List.hd list2 in
	match list1 with
	| [] -> failwith "Comparaison impossible"
	| h :: t when h < hd -> true
	| h :: t when h = hd -> inf_lex_list t (List.tl list2)
	| _ -> false;;
	
inf_lex_list [-1;2] [1;-5];;
inf_lex_list [1;2] [1;-5];;
inf_lex_list [1;2] [1;5];;
inf_lex_list [1;2;8;9] [1;-5;7];;
inf_lex_list [1] [2;0;1];;
inf_lex_list [3] [2;0;1];;
inf_lex_list [1] [1;0;1];;

(* Version du prof *)
let inf_lex (a,b) (c,d) = a < c || (a = c && b <= d);;
let rec inf_lex_list l1 l2 =
    match (l1,l2) with
        | [],[] -> true
        | [],_ -> failwith "Comparaison impossible"
        | _ ,[] -> failwith "Comparaison impossible"
        | (h1::t1), (h2::t2) -> h1 < h2 || (h1 = h2 && (inf_lex_list t1 t2));;

(*
* Exercice 6
*)

let selection filtre list =
	let rec supprime_rec list1 list2 =
		match list1 with
		| [] -> List.rev list2
		| h :: t when filtre h -> supprime_rec t (h :: list2)
		| h :: t -> supprime_rec t list2
	in supprime_rec list [];;
	
let est_pair x = x mod 2 = 0;;

selection est_pair [0;1;2;3;4;5;6;7;8;9;10];;

(*
* Exercice 7
*)

let rec intervalle_entier a b = 
	match a, b with 
	| a, b when a > b -> []
	| a, b -> a :: intervalle_entier (a+1) b;;

intervalle_entier 3 7;;
intervalle_entier 4 4;;
intervalle_entier 5 3;;
