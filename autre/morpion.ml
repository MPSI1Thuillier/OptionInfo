(*
* Morpion en ligne de commande, pour le fun
*
* Pour l'éxécuter :
* ocaml morpion.ml
*
* Pour le compiler :
* ocamlopt morpion.ml
*)

(*
* Cette première version est moche
* (elle est pensée itérative),
* mais elle fonctionne
*)

(* Classe qui représente une partie *)
class game player1 player2 =
  object (self)
    val player1 = player1
    val player2 = player2
    val table = Array.make_matrix 3 3 '*'

    method start =
      let win = ref '*' in
      while !win = '*' && not (self#full table) do begin
        let players = [|player1; player2|] in
        for k = 0 to 1 do begin
          if not (self#full table) && !win = '*' then begin
            let player = players.(k) in
            self#show();
            print_endline ("Au tour de " ^ (String.make 1 player#sign) ^ " de jouer !");
            let x = ref (-1) in
            let y = ref (-1) in
            while not (self#play !x !y player#sign) do begin
              let (xx, yy) = player#play self in
              x := xx;
              y := yy
            end done;
            let ys = string_of_int (!y+1) in
            let xs = string_of_int (!x+1) in
            print_endline ((String.make 1 player#sign) ^ " joue ligne " ^ ys ^ ", colonne " ^ xs);
            win := (self#win table)
          end
        end done
      end done;
      if !win = '*' then
        print_endline "Match nul !"
      else
        print_endline ((String.make 1 !win) ^ " remporte la partie !")
    
    method show =
      print_newline();
      print_string " ";
      for x = 0 to 2 do
        print_string " ";
        print_int (x+1)
      done;
      print_newline();
      for y = 0 to 2 do
        print_int (y+1);
        print_string " ";
        for x = 0 to 2 do
          print_char table.(x).(y);
          print_string " "
        done;
        print_newline()
      done;
      print_newline

    method play x y player =
      if x >= 0 && x < 3 && y >= 0 && y < 3 && table.(x).(y) = '*' then begin
        table.(x).(y) <- player;
        true
      end else false
    
    method win table =
      let r = ref '*' in
      let i = ref 0 in
      while !r = '*' && !i < 3 do begin
        r := self#line table !i;
        i := !i + 1
      end done;
      while !r = '*' && !i < 3 do begin
        r := self#col table !i;
        i := !i + 1
      end done;
      while !r = '*' && !i < 2 do begin
        r := self#dia table !i;
        i := !i + 1
      end done;
      !r

    method line table y =
      let player = ref table.(0).(y) in
      let changed = ref false in
      for x = 0 to 2 do
        if table.(x).(y) <> !player then changed := true
      done;
      if !changed then '*' else !player
    
    method col table x =
      let player = ref table.(x).(0) in
      let changed = ref false in
      for y = 0 to 2 do
        if table.(x).(y) <> !player then changed := true
      done;
      if !changed then '*' else !player

    method dia table d =
      let i = if d = 0 then 0 else 2 in
      let player = ref table.(i).(0) in
      let changed = ref false in
      for x = 0 to 2 do
        let i = if d = 0 then x else 2-x in
        if table.(i).(x) <> !player then changed := true
      done;
      if !changed then '*' else !player
    
    method full table =
      let f = ref true in
      for x = 0 to 2 do
        for y = 0 to 2 do
          if table.(x).(y) = '*' then f := false
        done
      done;
      !f
  end

(* Classe abstraite pour un joueur *)
class virtual player sign =
  object (self)
    val sign: char = sign

    method sign = sign
    method virtual play: game -> int * int
  end

(* Classe qui représente un humain *)
class human sign =
  object (self)
    inherit player sign as super

    method play game =
      print_string "Entrer le numéro de la ligne (1 à 3) : ";
      let y = int_of_string (read_line()) in
      print_string "Entrer le numéro de la colonne (1 à 3) : ";
      let x = int_of_string (read_line()) in
      (x-1, y-1)
  end

(* TODO: Implémenter une AI et un choix Joueur / AI *)

(* On lance la partie *)
let _ = (new game (new human 'X') (new human 'O'))#start