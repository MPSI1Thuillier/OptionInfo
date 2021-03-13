(* Exercice 1 *)

let matrice = [| [|1; 2; 3|]; [|4;5;6|] |] ;;

let somme t = 
	let s = ref 0 and li = Array.length t and co = Array.length t.(0) in
		for l = 0 to li-1 do 
			for c = 0 to co-1 do 
				s := !s + t.(l).(c)
			done;
		done;
		!s;;
		
somme matrice;;

let nbsup t (v:int) = 
	let nb = ref 0 and li = Array.length t and co = Array.length t.(0) in
		for l = 0 to li-1 do 
			for c = 0 to co-1 do 
				if t.(l).(c) > v then nb := !nb + 1
			done;
		done;
		!nb;;

nbsup matrice 2;;

let transpose t = 
	let li = Array.length t and co = Array.length t.(0) in
		let tr = Array.make_matrix co li t.(0).(0) in
		for l = 0 to li-1 do 
			for c = 0 to co-1 do 
				tr.(c).(l) <- t.(l).(c)
 			done;
		done;
		tr;;

transpose matrice;;		

(* Exercice 2 *)

let carte = [|	[| 0;		7		;6		;-1		;4			;5		|];
					[| 7;		0		;-1	;-1		;-1		;10	|];
					[| 6;		-1		;0		;5			;4			;10	|];
					[| -1;	-1		;5		;0			;-1		;6		|];
					[| 4;		-1		;4		;-1		;0			;-1	|];
					[| 5;		10		;10	;6			; -1		;0		|]|];;
					
let voisins c v = 
	let n = Array.length c in
		let rep = Array.make n true in 
			for i = 0 to n-1 do 
				if c.(i).(v) < 1 then rep.(i) <- false
			done;
	rep;;
	
voisins carte 0;;

let longueur c chem = 
	let l = ref 0 and n = Array.length chem in
		for i = 0 to n-2 do 
			if !l <> -1 then
				let d = c.(chem.(i)).(chem.(i+1)) in
			  		if d = -1
			  		then l := -1
			  		else l := !l + d
		done;
		!l;;
			 
longueur carte [|0;1;5|];;
longueur carte [|0;1;2|];;

let rec longueur c chem = 
	if Array.length chem < 2
	then 0
	else let d = c.(chem.(0)).(chem.(1)) in
		if d = -1
		then -1
		else let lg =  longueur c (Array.sub chem 1 (Array.length chem - 1)) in
			if lg = -1
			then -1
			else d +lg
;;


(* Exercice 3 *)

let rec fact n = 
	if n = 0
	then 1 
	else n*fact (n-1)
;;

let rec fact n = 
	match n with 
		| 0 -> 1
		| n -> n*fact (n-1)
;;

let rec fact = function  
		| 0 -> 1
		| n -> n*fact (n-1)
;;

fact 5;;

let rec puiss a = function 
	| 0 -> 1
	| n -> a * puiss a (n-1)
;;

puiss 2 3;;
(* Exercice 4 *)

function f -> f 0+1;;
function n -> (function x -> x + n);;
fun n f -> f (n + 1) - 1;;

(* Exercice 5 *)

let x y z = (z-3*y) = y;;

let f x y = x*y in f 4;;

(* Exercice 6 *)

let implique p q = (not p) || q;;

let implique a b = match (a,b) with 
	| (true,false) -> false
	| _ -> true
;;

let implique a = function 
	| true -> true
	| _ -> not a
;;

let implique = function 
	| true -> (function a->a)
	| false -> (function a->true)
;;

(implique false);;
let f = (implique false) in (f true);;
implique(false);;
implique true false;;
implique false false;;
implique (true false);;