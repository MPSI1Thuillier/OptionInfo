(*
* Opérations basiques sur les listes
*)

let list = 17::8::77::21::7::[];;

(* Tête de liste *)
List.hd list;;

(* Queue de liste *)
List.tl list;;

(* Opération récursive sur une liste *)
let rec afficher l =
	match l with
	| [] -> ()
	| h :: t -> print_int h; print_string " "; afficher t;;
	
afficher list;;

(* Reprogrammer List.length *)
let rec long = function
	| [] -> 0
	| h :: t -> 1 + long t;;
	
long [1;3;5];;

let long l =
	let rec long_rec c l =
		match l with
		| [] -> c
		| h :: t -> long_rec (c+1) t
	in long_rec 0 l;;
	
long [1;3;5];;

(* Reprogrammer List.rev *)
let miroir l =
	let rec miroir_rec n o =
		match o with
		| [] -> n
		| h :: t -> miroir_rec (h :: n) t
	in miroir_rec [] l;;
	
miroir [1;2;3];;

(* Concatenation *)
list @ list;;

let rec concatenation list1 list2 =
	match list1 with
	| [] -> list2
	| h :: t -> h :: (concatenation t list2);;

concatenation [1;2;3] [4;5;6];;

let concatenation list1 list2 =
	let rec concatenation_rec list1 list2 =
		match list1 with
		| [] -> list2
		| h :: t -> concatenation_rec t (h :: list2)
	in concatenation_rec (miroir list1) list2;;
	
concatenation [1;2;3] [4;5;6];;

(* List.mem *)
let rec contient e l =
	match l with
	| [] -> false
	| h :: t -> h = e || contient e t;;

contient 3 [2;3;4;5];;
contient 1 [2;3;4;5];;

(* List.iter *)
List.iter print_char ['a'; 'e'; 'i'; 'o'; 'u'];;

(* Carrés *)
let carres l =
	let carre n =
		print_int n;
		print_string "^2=";
		print_int (n*n);
		print_string " / "
	in List.iter carre l;;
	
carres [3;5;9;0];;

(* List.map *)
List.map (function x -> x*x) list;;

(* Le plus grand élément d'une liste *)
let max_liste l =
	let rec max_list_rec m l =
		match l with
		| [] -> m
		| h :: t -> max_list_rec (if h > m then h else m) t
	in max_list_rec (List.hd l) l;;
	
max_liste [9;23;2;5];;
