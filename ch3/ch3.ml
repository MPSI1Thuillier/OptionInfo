(*
* Chapitre 3 - Récursivité
* 
* https://www.youtube.com/watch?v=aaxO-7fXAjo
*)

(*
* Suite de fibonachi - Version itérative
*)

let fibo n =
    let u0 = ref 1 in
    let u1 = ref 1 in
    for k = 2 to n do
        let u2 = !u0 + !u1 in
        u0 := !u1;
        u1 := u2
    done; !u1

let () = assert ((fibo 5) = 8)

(*
* Suite de fibonachi - Version récursive naïve
*)

let rec fibo n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> fibo (n-1) + fibo (n-2)

let () = assert ((fibo 5) = 8)

(*
* Suite de fibonachi - Version récursive terminale
*)

let fibo n =
    let rec fibo_rec u0 u1 n =
        match n with
        | 0 -> u0
        | 1 -> u1
        | _ -> fibo_rec u1 (u0 + u1) (n-1)
    in fibo_rec 1 1 n

let () = assert ((fibo 5) = 8)
