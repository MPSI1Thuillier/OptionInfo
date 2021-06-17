(*
* Exercice 1
*)

let p = [|200; 100; 50; 20; 10; 5; 2; 1|]

let rendu (s: int): int array =
  let rec rendu_rec (s: int) (i: int) (result: int array) =
    match s with
    | 0 -> result
    | s when s < p.(i) -> rendu_rec s (i+1) result
    | s ->
      result.(i) <- result.(i) + 1;
      rendu_rec (s - p.(i)) i result
  in rendu_rec s 0 (Array.make (Array.length p) 0)

let _ = assert (rendu 132 = [|0; 1; 0; 1; 1; 0; 1; 0|])

(*
* Exercice 2
*)

let rec renduR2 (p: int array) (s: int): int =
  let rec minN (p: int array) (s: int) (i: int): int =
    match i with
    | i when i >= Array.length p -> -1
    | _ -> match minN p s (i+1) with
      | m when s - p.(i) < 0 -> m
      | m when m = -1 -> renduR2 p (s - p.(i))
      | m -> min (renduR2 p (s - p.(i))) m
  in match s with
  | 0 -> 0
  | s -> 1 + minN p s 0

let _ = assert (renduR2 [|1; 3; 4|] 6 = 2)

let rec renduR2 (p: int array) (s: int): int * int list =
  let rec minN (p: int array) (s: int) (i: int): int * int list =
    match i with
    | i when i >= Array.length p -> (-1, [])
    | _ -> match minN p s (i+1) with
      | m, e when s - p.(i) < 0 -> m, e
      | m, e when m = -1 ->
        let n, l = renduR2 p (s - p.(i))
        in (n, p.(i) :: l)
      | m, e ->
        let n, l = (renduR2 p (s - p.(i)))
        in if n < m then n, p.(i) :: l else m, e
  in match s with
  | 0 -> (0, [])
  | s -> let n, l = minN p s 0 in (1 + n, l)

let _ = assert (renduR2 [|1; 3; 4|] 6 = (2, [3; 3]))
