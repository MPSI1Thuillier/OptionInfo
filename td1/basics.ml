(* On fait une fonction puissance récursive *)
let rec puiss a b =
    (*
    On regarde si on est sur du puissance 0, donc a^0 = 1
    Dans les autres cas on return a * a^(b-1)
    *)
    match b with
    | 0 -> 1
    | _ -> a * puiss a  (b - 1);;

(* On test pour 2^3 *)
print_string "2^8 = ";;
print_int(puiss 2 3);;
print_newline();;

(* Fonction recursive pour les factorielles *)
let rec factorielle n =
    match n with
    | 0 -> 1
    | _ -> n * factorielle (n-1);;

(* On test pour 8! = 40320 *)
print_string "8! = ";;
print_int (factorielle 8);;
print_newline();;

(* Checker un multiple de 3 *)
let multiple a = if a mod 3 = 0 then 1 else 0;;

(* On test *)
print_string "8 multiple de 3 ? ";;
print_int (multiple 8);;
print_newline();;

(* Affiche un array d'integers *)
let print_array array =
    for i = 0 to Array.length(array) - 1 do
    print_int(array.(i));
    print_string("\t")
    done;;

(* On test avec un simple array *)
print_array [|1; 2; 3|];;
print_newline();;

(* Ecart entre le min et le max d'un array *)
let etendue array =
    let min = ref array.(0) in
    let max = ref array.(0) in
    for i = 0 to Array.length(array) -1 do
        if array.(i) < !min then min := array.(i)
        else if array.(i) > !max then max := array.(i)
    done;
    !max - !min;;

(* On test *)
print_string "Etendue = ";;
print_int (etendue [|2; 1; 4; 3|]);;
print_newline();;

(* Fonction modale - Je reviens dessus après *)

(* Un petit triangle de pascal *)
let pascal n =
    (* On définit la ligne de base *)
    let line = ref [|1|] in

    (* On iterate pour les lignes suivantes *)
    for i = 1 to n do
        let newline = Array.make (i + 1) 1 in
        for k = 0 to i do
            (*
            On calcul les coefs en fonction de la ligne précédente
            Sachant que sur les bords on a des 1
            *)
            newline.(k) <- if k = 0 || k = i then 1 else !line.(k) + !line.(k - 1)
        done;

        (* On enregistre et on affiche les étapes *)
        line := newline;
        print_array !line;
        print_newline();
    done;
    !line;;

(* On affiche le triangle de Pascalou *)
print_string "Pascalou :";;
print_newline();;
print_array(pascal 5);;
print_newline();;

(* Trouver la première occurence d'une lettre dans une string *)
let find_chr txt need =
    let index = ref (-1) in (* Ne pas oublier les paranthèses sinon il considère qu'on soustrait 1 à ref *)
    for i = 0 to String.length(txt) - 1 do
        if !index = -1 && txt.[i] = need then index := i
    done;
    !index;;

(* On essaye *)
print_string "Index du premier o dans \"bonjour popol !\" : ";;
print_int (find_chr "bonjour popol !" 'o');;
print_newline();;

(* Le palindrome (oui encore) *)
let palindrome str =
    (* Le nom de cette variable est une dédicasse à notre ami Popol *)
    let laTableBleuQuiRouilleAuFondDuJardin = ref false in
    let len = String.length(str) in
    for i = 0 to len - 1 do
        if not !laTableBleuQuiRouilleAuFondDuJardin && str.[i] = str.[len - i - 1] then laTableBleuQuiRouilleAuFondDuJardin := true
    done;
    !laTableBleuQuiRouilleAuFondDuJardin;;

(* On essaye *)
print_string "Palindrome : ";;
print_int (if palindrome "popol" then 1 else 0);;
print_int (if palindrome "kayak" then 1 else 0);;
print_newline();;

(* On passe les voyelles en majuscule *)
let voyelles str =
    let result = ref "" in
    for i = 0 to String.length(str) - 1 do
        let char = str.[i] in
        if char = 'a' || char = 'e' || char = 'i' || char = 'o' || char = 'u'
        then result := !result ^ String.make 1 (Char.uppercase_ascii char)
        else result := !result ^ String.make 1 char
    done;
    !result;;

(* On essaye *)
print_string (voyelles "bonjour charles on fait le td de ce mardi");;
print_newline();;

(* ET VOILA *)
