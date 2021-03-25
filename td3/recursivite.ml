(*
* Exercice 1
*)

let rec palindrome str =
  match String.length str with
  | 1 -> true
  | 2 -> str.[0] = str.[1]
  | n -> str.[0] = str.[n-1] && palindrome (String.sub str 1 (n-2))

let _ = assert (palindrome "kayak")
let _ = assert (palindrome "abba")
let _ = assert (not (palindrome "tokyo"))

(*
* Exercice 2
*)

let rec est_trie t =
  match Array.length t with
  | 0 | 1 -> true
  | n -> t.(0) <= t.(1) && est_trie (Array.sub t 1 (n-1))

let _ = assert (est_trie [|1; 2; 3; 4; 5|])
let _ = assert (not (est_trie [|1; 2; 7; 4; 5|]))

(*
* Exercice 3
*)

let rec pgcd a b =
  match b with
  | 0 -> a
  | _ -> pgcd b (a mod b)

let _ = assert ((pgcd 2 3) = 1)
let _ = assert ((pgcd 9 3) = 3)

let nombre_couples n =
  let rec nombre_couples_rec c a b =
    match b with
    | 0 -> c
    | _ -> nombre_couples_rec (c + (if (pgcd a b) = 1 then 1 else 0)) a (b-1)
  in let rec nombre_couples_rec2 c a =
    match a with
    | 0 -> c
    | _ -> nombre_couples_rec2 (c + (nombre_couples_rec 0 a n)) (a-1)
  in nombre_couples_rec2 0 n

(*
* Pour n = 3, on a (1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3)
* donc 7 couples premiers
*)

let _ = assert ((nombre_couples 3) = 7)

(*
* Exercice 4
*)

(* Récupération de la fontion estPremier du dernier DS *)

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

let decompose n =
  let rec decompose_rec n a =
    match n with
    | 1 -> print_newline()
    | n when estPremier a && n mod a = 0 ->
      print_int a;
      print_string " ";
      decompose_rec (n/a) a
    | _ -> decompose_rec n (a+1)
  in decompose_rec n 1

let _ = decompose 36

(*
* Exercice 5
*)

let rec f n =
  match n with
  | n when n > 100 -> n - 10
  | _ -> f (f (n+11))

(* ... *)

(*
* Exercice 6
*)

let rec terme f a n =
  match n with
  | 0 -> a
  | _ -> f (terme f a (n-1))

let _ = assert ((terme (fun x -> x*2) 1 3) = 8)

(*
* Exercice 7
*)

let print_partition t =
  print_string ("{" ^ (String.concat ", " (List.map (fun x -> string_of_int x) t)) ^ "} ")

let partition n = 
  let rec partition_rec t n =
    match n with
    | 0 -> print_newline()
    | _ -> print_partition (t @ n :: []); partition_rec (1 :: t) (n-1)
  in partition_rec [] n

(*
* A terminer, il manque des cas
* A voir si c'est pas mieux de faire décroitre le premier nombre et
* de chercher les partitions du reste
*)

let _ = partition 5

(*
* Exercice 8
*)

let dichotomie f a b e =
  let rec dichotomie_rec f a b e =
    let m = (b +. a) /. 2.0 in
    match b -. a with
    | d when d <= e -> (a, b)
    | _ when (f a) *. (f m) < 0.0 -> dichotomie_rec f a m e
    | _ when (f a) *. (f m) > 0.0 -> dichotomie_rec f m b e
    | _ -> (a, b)
  in let (fa, fb) = dichotomie_rec f a b e in
  (fa +. fb) /. 2.0

let _ = print_endline (string_of_float (dichotomie (fun x -> x *. x -. 2.0) 1.0 2.0 0.0001))
