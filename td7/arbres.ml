(*
* TD7 : Arbres
*)

let print_bool (boolean: bool): unit =
  match boolean with
  | true -> print_string "true"
  | false -> print_string "false"

type tree =
  | Leaf
  | Node of int * tree * tree

let test_tree = Node(
  5,
  Node(
    3,
    Node(
      2,
      Node(
        1,
        Leaf,
        Leaf
      ),
      Node(
        4,
        Leaf,
        Leaf
      )
    ),
    Leaf
  ),
  Node(
    8,
    Node(
      7,
      Leaf,
      Leaf
    ),
    Node(
      10,
      Leaf,
      Leaf
    )
  )
)

(*
* Exercice 1
*)

let () = print_endline "-- Exercice 1 --"

let rec height (tree: tree): int =
  match tree with
  | Leaf -> 0
  | Node (_, left, right) ->
    1 + max (height left) (height right)

let rec is_stable (tree: tree): bool =
  match tree with
  | Leaf -> true
  | Node (_, left, right) ->
    (is_stable left || is_stable right)
    && abs ((height left) - (height right)) <= 1

let () = print_bool (is_stable test_tree)
let () = print_newline()

(*
* Exercice 2
*)

let () = print_endline "-- Exercice 2 --"

let rec print_tree_left_prefixed (tree: tree): unit =
  match tree with
  | Leaf -> ()
  | Node (value, left, right) ->
    print_int value;
    print_string " ";
    print_tree_left_prefixed left;
    print_tree_left_prefixed right

let rec print_tree_left_infixed (tree: tree): unit =
  match tree with
  | Leaf -> ()
  | Node (value, left, right) ->
    print_tree_left_infixed left;
    print_string " ";
    print_int value;
    print_string " ";
    print_tree_left_infixed right

let rec print_tree_left_suffixed (tree: tree): unit =
  match tree with
  | Leaf -> ()
  | Node (value, left, right) ->
    print_tree_left_suffixed left;
    print_tree_left_suffixed right;
    print_string " ";
    print_int value

let () = print_tree_left_prefixed test_tree
let () = print_newline()
let () = print_tree_left_infixed test_tree
let () = print_newline()
let () = print_tree_left_suffixed test_tree
let () = print_newline()

(*
* Exercice 3
*)

let () = print_endline "-- Exercice 3 --"

let merge_lists (list1: int list list) (list2: int list list): int list list =
  let rec merge_lists_rec (list1: int list list) (list2: int list list) (result: int list list): int list list =
    match list1, list2 with
    | h1 :: t1, h2 :: t2 -> merge_lists_rec t1 t2 ((h1 @ h2) :: result)
    | h1 :: t1, [] -> merge_lists_rec t1 [] (h1 :: result)
    | _, h1 :: t1 -> merge_lists_rec t1 [] (h1 :: result)
    | [], [] -> List.rev result
  in merge_lists_rec list1 list2 []

let rec list_of_tree (tree: tree): int list list =
  match tree with
  | Leaf -> []
  | Node (value, left, right) ->
    [value] :: merge_lists (list_of_tree left) (list_of_tree right)

let print_tree_by_line (tree: tree): unit =
  let rec print_next_int (list: int list) =
    match list with
    | h :: t -> print_int h; print_string " "; print_next_int t
    | [] -> ()
  in
  let rec print_next_line (list: int list list) =
    match list with
    | h :: t -> print_string "[ "; print_next_int h; print_string "]\n"; print_next_line t
    | [] -> ()
  in print_next_line (list_of_tree tree)

let () = print_tree_by_line test_tree
let () = print_newline()

(*
* Exercice 4
*)

let () = print_endline "-- Exercice 4 --"

let rec is_binary_search_tree (tree: tree) =
  match tree with
  | Leaf -> true
  | Node (value, left, right) ->
    (match left with
    | Leaf -> true
    | Node (value_left, _, _) ->
      value_left <= value)
    && (match right with
    | Leaf -> true
    | Node (value_right, _, _) ->
      value_right >= value)
    && is_binary_search_tree left
    && is_binary_search_tree right

let () = print_bool (is_binary_search_tree test_tree)
let () = print_newline()

let rec contains (tree: tree) (value: int) =
  match tree with
  | Leaf -> false
  | Node (current, left, right) ->
    current = value
    || contains left value
    || contains right value

let () = print_bool (contains test_tree 10)
let () = print_newline()

let () = print_bool (contains test_tree 13)
let () = print_newline()
