let sum_array mat = Array.fold_left (+) 0 (Array.map (Array.fold_left (+) 0) mat)

let nb_sup mat a = List.fold_left (+) 0 (List.map (List.length) (List.map (List.filter (fun x -> x > a)) (Array.to_list (Array.map (fun x -> Array.to_list x) mat))))

let transpose mat = Array.init (Array.length mat) (fun x -> Array.init (Array.length mat.(x)) (fun y -> mat.(y).(x)))


let neighbours mat a = Array.map (fun x -> x > 0) mat.(a)

let length mat arr =
  let path = Array.map2 (fun x y -> mat.(x).(y)) (Array.sub arr 0 (Array.length arr - 1)) (Array.init (Array.length arr - 1) (fun x -> arr.(x+1))) in
  if Array.mem (-1) path then -1
  else Array.fold_left (+) 0 path

let length_rec mat arr =
  let rec aux arr prev acc =
    match arr with
    | hd :: _ when mat.(prev).(hd) = -1 -> -1
    | hd :: tl -> aux tl hd (acc + mat.(prev).(hd)) 
    | _ -> acc
  in
  aux (Array.to_list (Array.sub arr 1 (Array.length arr - 1))) arr.(0) 0

let rec power a n =
  match n with
  | 0 -> 1
  | 1 -> a
  | x -> a * power a (x-1)

let rec factorial a =
  match a with
  | 0 | 1 -> 1
  | x -> x * factorial (x-1)


let exemple1 f = (f 0) + 1
let exemple2 a b = a + b
let exemple3 a f = (f 0) + 1 + a;;

(* Exemple 1 *)
(* int -> int *)
(* On obtient ainsi une fonction int -> int -> int, à laquelle on passe 4 pour le premier paramètre, on obtient donc fun x -> 4 * x *)
(* Exemple 2 *)
(* val x = int -> int -> bool *)
(* On obtient une fonction x qui vérifie si z - 3 * y = y et retourne true ou false *)


let implies a b = (not a) || b

let implies_filtr a b =
  match a, b with
  | true, false -> false
  | _ -> true

(* 1 = bool -> bool *)
(* 2 = true *)
(* 3 = bool -> bool *)
(* 4 = false *)
(* 5 = true *)
(* 6 = error *)


let adj = [|[|0; 7; 6; -1; 4; 6|];
            [|7; 0; -1; -1; -1; 10|];
            [|6; -1; 0; 5; 4; 10|];
            [|-1; -1; 5; 0; -1; 6|];
            [|4; -1; 4; -1; 0; -1|];
            [|6; 10; 10; 6; -1; 0|]|]

let () =  Printf.printf "%d" (length_rec adj [|0; 3; 2; 5|])