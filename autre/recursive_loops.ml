(*
* Redéfinition des boucles en recursive
*)

(*
* Définition
*)
let rec recursive_while (condition: unit -> bool) (execute: unit -> unit) =
  match condition() with
  | true -> execute(); recursive_while condition execute
  | false -> ()

let rec recursive_for (start: int) (stop: int) (execute: int -> unit) =
  match start = stop with
  | true -> execute(start)
  | false -> execute(start); recursive_for (start + 1) (stop) execute

(*
* While
*)

(* Boucle normale *)
let () =
let a = ref 0 in
while !a < 5 do
  print_int !a;
  a := !a + 1
done;
print_newline()

(* Boucle recursive *)
let () =
let b = ref 0 in
recursive_while (fun () -> !b < 5) (fun () -> print_int !b; b := !b + 1);
print_newline()

(*
* For
*)

(* Boucle normale *)
let () =
for k = 0 to 4 do
  print_int k
done;
print_newline()

(* Boucle recursive *)
let () =
recursive_for 0 4 (fun k -> print_int k);
print_newline()
