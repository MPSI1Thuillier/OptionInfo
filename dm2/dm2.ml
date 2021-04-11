(*
* Exercice 1
*)

type frac = {num: int; den: int};;

let a = {num = 2; den = 3};;
let b = {num = -1; den = 1};;
let c = {num = 6; den = 10};;

a.num;;

let double x = {num = 2 * x.num; den = x.den};;

double a;;

(* PGCD *)

let rec pgcd a b =
	match b with
	| 0 -> a
	| _ -> pgcd b (a mod b);;
	
pgcd 4 15;;
pgcd (-12) 15;;

(* Simplifier une fraction *)

let simplifie a =
	let factor = pgcd a.num a.den in
	{num = a.num / factor; den = a.den / factor};;
	
simplifie c;;

(* Afficher proprement une fraction *)

let print_frac a =
	let simple = simplifie a in
	print_int simple.num;
	if simple.den <> 1 then begin
		print_string "/";
		print_int simple.den
	end;
	print_newline();;
	
print_frac a;;
print_frac b;;

(* Somme de deux fractions *)

let sommeQ a b =
	simplifie {
		num = a.num * b.den + a.den * b.num;
		den = a.den * b.den
	};;
let (++) = sommeQ;;

a ++ b;;
a ++ c;;

(* Produit de deux fractions *)

let produitQ a b =
	simplifie {
		num = a.num * b.num;
		den = a.den * b.den;
	};;
let ( ** ) = produitQ;;

a ** b;;
a ** c;;

(* Différence de deux fractions *)

let differenceQ a b =
	simplifie {
		num = a.num * b.den - a.den * b.num;
		den = a.den * b.den
	};;
let (--) = differenceQ;;

a -- b;;
a -- c;;

(* Quotient de deux fractions *)

let quotientQ a b =
	simplifie {
		num = a.num * b.den;
		den = a.den * b.num;
	};;
let (//) = quotientQ;;

a // b;;
a // c;;

(* Fraction à partir d'une string *)

let frac_of_string str =
	let parts = String.split_on_char '/' str in
	if List.length parts = 2 then
		{
			num = int_of_string (List.hd parts);
			den = int_of_string (List.hd (List.tl parts))
		}
	else
		{
			num = int_of_string (List.hd parts);
			den = 1
		};;
		
(* Deuxième version, mieux que la première à mon gout *)

let frac_of_string str =
	match String.split_on_char '/' str with
	| [] -> failwith "Fraction invalide"
	| h :: t when t = [] -> {
			num = int_of_string h;
			den = 1
		}
	| h :: t -> {
			num = int_of_string h;
			den = int_of_string (List.hd t)
		};;
		
let f = frac_of_string;;

f "1";;
f "-2/3";;

(* Evaluation d'une faction *)

let eval x p =
	let num = float_of_int x.num in
	let den = float_of_int x.den in
	let v = num /. den in
	Printf.printf ("%.2f") v;;
	(*
	A voir comment intégrer p,
	J'ai essayé ça mais ça ne marche pas :
	Printf.printf ("%." ^ (string_of_int p) ^ "f") v
	*)
	
eval (f "2020/5") 0;;
eval (f "-111/19") 20;;

(*
* Exercice 2
*)

let p1 = [];; (* Polyn^ome nul *)
let p2 = [f "2" ; f "1" ; f "4"];; (* P2(X) = 4X^2+X+2 *)

(* Degré d'un polynome *)

let degre p = (List.length p) - 1;;
	
degre p1;;
degre p2;;

(* Affichage d'un polynome *)

let affiche p =
	let rec print_element l i =
		match l with
		| [] -> print_newline()
		| h :: t ->
			if i <> 0 then print_string " + ";
			print_frac h;
			if i <> 0 then begin
				print_string "X^";
				print_int i;
			end;
			print_element t (i+1)
	in print_element p 0;;

affiche p2;;

(* Image d'une fraction par le polynome *)

let image a p =
	let rec image_rec r p =
		match p with
		| [] -> r
		| h :: t -> image_rec (h ++ a ** r) t
	in image_rec (f "0") (List.rev p);;
	
image (f "3/2") p2;;
image (f "5") p1;;

(* Produit Xn *)

let rec produitXn p n =
	match p with
	| [] -> []
	| l when n = 0 -> p
	| l -> produitXn ((f "0") :: l) (n-1);;
	
produitXn p1 3;;
affiche (produitXn p2 2);;

(* Somme de deux polynomes *)

let simplifie p =
	let rec simplifie_rec p =
		match p with
		| h :: t when h = f "0" -> simplifie_rec t
		| _ -> p
	in List.rev (simplifie_rec (List.rev p));;

simplifie [f "0"];;
affiche (simplifie [f "1"; f "2"; f "0"; f "3"; f "0"]);;

let somme p1 p2 =
	let rec somme_rec l p1 p2 =
		match p1, p2 with
		| [], [] -> List.rev l
		| h :: t, [] -> somme_rec (h :: l) t p2
		| [], p2 -> somme_rec l p2 p1
		| h1 :: t1, h2 :: t2 -> somme_rec ((h1 ++ h2) :: l) t1 t2
	in simplifie (somme_rec [] p1 p2);;
		
	
affiche (somme p2 [f"-2";f"1";f"-4"]);;
affiche (somme p2 [f"1";f"2"]);;

(* Dérivée d'un polynome *)

let derive p =
	let rec derive_rec n d p =
		match p with
		| h :: t when n = 0 -> derive_rec (n+1) d t
		| h :: t -> derive_rec (n+1) (({num = n; den = 1} ** h) :: d) t
		| [] -> List.rev d
	in derive_rec 0 [] p;;

affiche (derive p2);;
derive p1;;

(* Produit par un nombre *)

let produit_nb p a =
	let rec produit_nb_rec p a r =
		match p with
		| [] -> List.rev r
		| h :: t -> produit_nb_rec t a ((h ** a) :: r)
	in produit_nb_rec p a [];;

affiche (produit_nb p2 (f"2/3"));;
produit_nb p1 (f"3");;

(* Produit de deux polynomes *)

let produit p1 p2 =
	let rec produit_rec p1 p2 r n =
		match p1 with
		| [] -> r
		| h :: t ->
			let p = produitXn (produit_nb p2 h) n in
			produit_rec t p2 (somme r p) (n+1)
	in produit_rec p1 p2 [] 0;;

affiche (produit p2 p2);;
produit p2 p1;;

(* Coefficient dominant *)

let rec coeff_dom p =
	match p with
	| [] -> f "0"
	| h :: t when t = [] -> h
	| h :: t -> coeff_dom t;;

coeff_dom p1;;
coeff_dom p2;;

(* Reste de la division euclidienne *)

let rec reste p1 p2 =
	match degre p1, degre p2 with
	| d1, d2 when d1 >= d2 ->
		let q = produitXn [((coeff_dom p1) // (coeff_dom p2))] (d1 - d2) in
		let s = produit q p2 in
		reste (somme p1 (produit_nb s (f "-1"))) p2
	| _, _ -> p1;;

reste p2 [f"-1"; f"2"];;

(*
* Exercice 3
*)

(* Chaine de Strum *)

let sturm p =
	let rec sturm_rec p0 p1 l =
		let p2 = produit_nb (reste p0 p1) (f "-1") in
		match p2 with
		| [] -> l
		| _ -> sturm_rec p1 p2 (p2 :: l) in
	let p1 = derive p in
	sturm_rec p p1 [p1; p];;

List.iter affiche (sturm [f"-1" ; f"-1" ; f"0" ; f"1" ; f"1"]);;

(* Changement de signe *)

let signes l =
	let rec signes_rec l e c =
		match l with
		| [] -> c
		| h :: t when h = f "0" -> signes_rec t e c
		| h :: t when let he = h ** e in he.num * he.den < 0 -> signes_rec t h (c+1)
		| h :: t -> signes_rec t h c
	in signes_rec l (f "0") 0;;
	
signes [f"1" ; f"-1" ; f"2" ; f"-4"];;
signes [f"1" ; f"-1" ; f"2" ; f"0" ; f"-4"];;
signes [f"1" ; f"-1" ; f"-2" ; f"0" ; f"-4"];;

(* Nombre de racines *)

let sigma l x =
	let images = List.map (image x) l in
	signes images;;

let nb_racines p a b =
	let l = sturm p in
	let sa = sigma l a in
	let sb = sigma l b in
	sa - sb;;

nb_racines [f"-4";f"1";f"4"] (f"-2") (f"2");;
nb_racines [f"-4";f"1";f"4"] (f"-2") (f"-1");;

(* Borne *)

let borne p =
	let cd = coeff_dom p in
	let np = produit_nb p {num = cd.den; den = cd.num} in
	let rec borne_rec p m =
		match p with
		| [] -> m
		| h :: t when let hm = h -- m in hm.num * hm.den > 0 -> borne_rec t h
		| h :: t -> borne_rec t m
	in (borne_rec np (f "0")) ++ (f "1");;

borne p2;;

(* Isole *)

let isole p =
	let majo = borne p in
	let mino = (f "-1") ** majo in
	let rec borne_racine p mino majo =
		match nb_racines p mino majo with
		| 0 -> []
		| 1 -> [majo]
		| _ ->
			let milieu = (mino ++ majo) // (f "2") in
			(borne_racine p mino milieu) @ (borne_racine p milieu majo)
	in mino :: (borne_racine p mino majo);;

(* Racines : -3, 1, 5 *)
List.iter print_frac (isole [f"15";f"-13";f"-3";f"1"]);;

(* Racines approchées à epsilon près *)

let racines p e =
	let majo = borne p in
	let mino = (f "-1") ** majo in
	let rec borne_racine p mino majo =
		let milieu = (mino ++ majo) // (f "2") in
		match nb_racines p mino majo with
		| 0 -> []
		| 1 when let d = majo -- mino -- e in d.num * d.den < 0 -> [milieu]
		| _ -> (borne_racine p mino milieu) @ (borne_racine p milieu majo)
	in (borne_racine p mino majo);;

let print_approx frac = eval frac 2; print_newline();;

(* Racines : -3, 1, 5 *)
List.iter (print_approx) (racines [f"15";f"-13";f"-3";f"1"] (f"1"));;
