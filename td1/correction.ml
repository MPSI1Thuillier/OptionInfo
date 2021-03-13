(***********************************************
* 						QUELQUES FONCTIONS    		  *
***********************************************)


(* Exercice 1 *) 

let puiss a b = 
    let p = ref 1 in 
      for i = 1 to b do 
        p := !p * a;
    done;
  !p;;

let rec puiss a b = 
	if b = 0
	then 1
	else a*(puiss a (b-1))
;;

(* Exercice 2 *) 

let fact n = 
  let p = ref 1 in 
    for i = 1 to n do 
      p := !p * i
    done;
    !p;;
    
let rec fact n = 
	if n = 0
	then 1
	else n*(fact (n-1))
;; 

for i = 0 to 21 do 
	print_int (fact i);
	print_string ";"
	done;; 
	
fact 21;;  

let fact n = 
   let f = [|1;1;2;6;24;120;720;5040;40320;362880;3628800;39916800;
   479001600;6227020800;87178291200;1307674368000;20922789888000;
   355687428096000;6402373705728000;121645100408832000;
   2432902008176640000|] in f.(n);;
   
fact 5;;

(* Exercice 3 *)

let multiple a = if a mod 3 = 0 then 1 else 0;;

let multiple a = let b = a mod 3 in (b-1)*(b-2)/2;;

(***********************************************
* 						TABLEAUX							  *
***********************************************)

(* Exercice 4 *)

let print_vect t = 
	for i = 0 to Array.length t - 1 do 
		print_int t.(i);
		print_char (char_of_int 9);
	done;
	print_newline();;
	
print_vect [|1;2;3|];;

(* Exercice 5 *)

let etendue v = 
	let mini = ref v.(0) and maxi = ref v.(0) in 
		for i = 0 to Array.length v - 1 do begin
			if v.(i) > !maxi then maxi := v.(i);
			if v.(i) < !mini then mini := v.(i);
		end done;
		!maxi - !mini
;;

etendue [|1;12;3|];;

(* Exercice 6 *)

let modale t = 
	let nb_max = ref 0 and v_max = ref t.(0) and nb_fois t v = 
		let nb = ref 0 in 
		for i = 0 to Array.length t - 1 do
			if t.(i) = v then nb := !nb + 1
		done;
		!nb in
	for i = 0 to Array.length t - 1 do 
		let x = nb_fois t t.(i) in
		if x > !nb_max then begin
			nb_max := x;
			v_max := t.(i)
		end
	done; 
	! v_max;;
	
modale [|'a' ; 'b' ; 'a' ; 'b' ; 'c' ; 'b' |];;

(* Exercice 7 *)

let pascal n = 
	let rep = Array.make (n+1) 0 in
	rep.(0) <- 1;
	for ligne = 1 to n do 
		for colonne = ligne downto 1 do 
			rep.(colonne) <- rep.(colonne)+rep.(colonne-1)
		done;
		print_vect rep;
	done;
   rep
;;

pascal 4;; 

(***********************************************
* 						CHAINES DE CARACTERES    	  *
***********************************************)

(* Exercice 8 *)

let find_chr txt need = 
  let ret = ref (-1) and i = ref 0  in 
    while !i < String.length txt && !ret = -1 do
      if txt.[!i] = need then ret := !i;
      i := !i + 1;
    done;
  !ret;;
  
 find_chr "maison" 'r';;
 
(* Exercice 9 *)

let palindrome mot = 
  let nb = ref 0 and l = String.length mot - 1 in
    for i = 0 to l / 2 do
      if mot.[i] <> mot.[l-i] then nb := !nb + 1;
    done;
    !nb = 0;;

let palindrome mot = 
  let nb = ref true and l = String.length mot - 1 and i = ref 0 in
    while (!i <= l / 2) && !nb do 
      if mot.[!i] <> mot.[l - !i] then nb := false;
      i := !i + 1;
    done;
    !nb;;
    
(* Exercice 10 *)

let voyelle txt = 
	let b_txt = Bytes.of_string txt in
		for i = 0 to Bytes.length(b_txt) -1 do 
      	let c = Bytes.get b_txt i in 
        	if (c='a') || (c='e') ||(c='i') ||(c='o') ||(c='u') then 
         	Bytes.set b_txt i (char_of_int(int_of_char(c)-32));
  done;
  Bytes.to_string b_txt;; 

let voyelle txt = 
	let b_txt = Bytes.of_string txt in
		for i = 0 to Bytes.length(b_txt) -1 do 
      	let c = Bytes.get b_txt i in 
        	if String.contains "aeiuoy" c
        	then Bytes.set b_txt i (Char.uppercase_ascii c);
  done;
  Bytes.to_string b_txt;; 

let voyelles txt = 
	let voyelle_maj c = 
		if String.contains "aeiouy" c
		then Char.uppercase_ascii c else c
	in String.map voyelle_maj txt;; 
	
voyelle "maison";; 
 