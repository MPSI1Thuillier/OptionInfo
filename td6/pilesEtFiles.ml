(*
* Exercice 1
*)

let liste_to_pile l =
	let rec ltp_rec l s =
		match l with
		| [] -> s
		| h :: t ->
			Stack.push h s;
			ltp_rec t s
	in ltp_rec (List.rev l) (Stack.create());;

let s = liste_to_pile [1;2;3];;

(*
* Exercice 2
*)

let remplis n a b =
	let rec remplis_rec s n a b =
		match n with
		| 0 -> s
		| _ ->
			Stack.push ((Random.int (b-a+1)) + a) s;
			remplis_rec s (n-1) a b
	in remplis_rec (Stack.create()) n a b;;

let rs = remplis 3 5 10;;
Stack.pop_opt rs;;
Stack.pop_opt rs;;
Stack.pop_opt rs;;
Stack.pop_opt rs;; (* None *)

(* Version initiale *)
let affiche p =
	for i = 1 to Stack.length p do
		print_int (Stack.pop p);
		print_string " <- "
	done;;
	
affiche s;;

(* Version modifiée *)
let affiche p =
	let copy = Stack.copy p in
	let result = ref "" in
	for i = 1 to Stack.length copy do
		let i = Stack.pop copy in
		result := (string_of_int i) ^ " <- " ^ !result
	done;
	print_string !result;;

let s = liste_to_pile [1;2;3];;
affiche s;;

(*
* Exercice 3
*)

let rec paire s p i =
	match Stack.pop_opt s with
	| Some n when n mod 2 = 0 ->
		Stack.push n p;
		paire s p i
	| Some n  ->
		Stack.push n i;
		paire s p i
	| None -> ();;
	
let s = liste_to_pile [1;3;4;6;5];;
let p = Stack.create();;
let i = Stack.create();;
paire s p i;;
affiche p;;
affiche i;;

(*
* Exercice 4
*)

let alterne s =
	let rec alternat_rec s p i b =
		match b, Stack.is_empty p, Stack.is_empty i with
		| true, false, _ ->
			Stack.push (Stack.pop p) s;
			alternat_rec s p i false
		| false, _, false ->
			Stack.push (Stack.pop i) s;
			alternat_rec s p i true
		| _, true, false ->
			Stack.push (Stack.pop i) s;
			alternat_rec s p i true
		| _, false, true ->
			Stack.push (Stack.pop p) s;
			alternat_rec s p i true
		| _, true, true ->
			s
	in let copy = Stack.copy s
	in let p = Stack.create()
	in let i = Stack.create()
	in paire copy p i;
	alternat_rec copy p i true;;
	
affiche (alterne (liste_to_pile [1;3;5;7;9;2;4;6;8;10]));;
affiche (alterne (liste_to_pile [1;3;5;7;9;2;4;6;8;10;11;13]));;
affiche (alterne (liste_to_pile [1;3;5;7;9;2;4;6;8;10;12;14]));;

(*
* Exercice 5
*)

let laby = [|
    13;  1;  3; 11; 13;  1;  1;  5;  5;  1;  3; 11; 13;  5;  3; 11;
     9;  2; 14;  8;  3; 14; 12;  5;  3; 14; 12;  6;  9;  5;  2; 10;
    14;  8;  3; 14; 12;  3;  9;  5;  0;  1;  5;  3;  8;  7;  8;  2;
    11; 14;  8;  7; 13;  2; 14; 13;  2; 14;  9;  0;  4;  7; 10; 14;
     8;  5;  2; 13;  5;  0;  3; 13;  2; 13;  6; 12;  1;  7; 12;  3;
    12;  3;  8;  1;  1;  2;  8;  5;  6; 11; 11; 11; 12;  7;  9;  2;
    13;  2; 14; 14; 10; 10;  8;  7; 11; 10;  8;  2;  9;  5;  6; 10;
     9;  6; 13;  3; 14; 10; 14;  9;  4;  4;  2; 14; 14; 13;  1;  6;
     8;  5;  7;  8;  3; 12;  1;  4;  7; 11; 14; 13;  3; 11; 12;  7;
    14; 11; 13;  2; 12;  5;  0;  5;  3;  8;  5;  1;  6; 12;  3; 11;
     9;  4;  7; 12;  3; 11; 10; 13;  6;  8;  7;  8;  1;  5;  4;  2;
    10; 11;  9;  5;  4;  2; 14;  9;  1;  2;  9;  2; 12;  1;  7; 10;
     8;  0;  4;  3;  9;  4;  5;  2; 10; 10; 10;  8;  7; 12;  3; 14;
    14; 14; 13;  6; 12;  3; 11; 14; 10; 14; 14; 14;  9;  7; 10; 11;
    11;  9;  7; 11; 13;  0;  4;  7; 12;  1;  5;  7; 10;  9;  0;  6;
    12;  4;  5;  4;  5;  4;  5;  7; 13;  4;  7; 13;  4;  6; 12;  7
|];;

let mur c d = c land d <> 0;;
mur 9 1;;
mur 9 2;;
mur 9 4;;
mur 9 8;;

(* Méthode naive *)

let solve d a =
	let extract_str str1 str2 str3 str4 =
		match str1, str2, str3, str4 with
		| "", "", "", _ -> str4
		| "", "", _, "" -> str3
		| "", _, "", "" -> str2
		| _, "", "", "" -> str1
		| _, _, _, _ -> failwith "Chemin pas unique"
	in let rec solve_rec d a n temp str =
		temp.(d) <- n;
		if d = a then str else begin
		extract_str
		(if laby.(d) land 1 = 0
			&& temp.(d-16) = -1 then
			solve_rec (d-16) a (n+1) temp (str ^ "H")
		else "")
		(if laby.(d) land 2 = 0
			&& temp.(d+1) = -1 then
			solve_rec (d+1) a (n+1) temp (str ^ "D")
		else "")
		(if laby.(d) land 4 = 0
			&& temp.(d+16) = -1 then
			solve_rec (d+16) a (n+1) temp (str ^ "B")
		else "")
		(if laby.(d) land 8 = 0
			&& temp.(d-1) = -1 then
			solve_rec (d-1) a (n+1) temp (str ^ "G")
		else "")
		end
	in let temp = Array.make 256 (-1)
	in solve_rec d a 0 temp "";;

solve 146 196;;
