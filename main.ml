(*
--------------------------------------------------------------------------------------------------------------
|                                                                                                            |
|                                                                                                            |
|                                            Made by ZirconiumZr40                                           |
|                                                   06/2021                                                  |
|                                       You can contact me on Twitter :                                      |
|                                       https://twitter.com/zirconium_60                                     |
|                                                                                                            |
|                                                                                                            |
--------------------------------------------------------------------------------------------------------------
*)
let print_int_array t =
  let n = ((Array.length t)-1) in
  print_string "[| " ;
  for i = 0 to (n-1) do
    Printf.printf "%1d ; " t.(i)
  done ;
  print_int t.(n) ; print_string " |]"
let print_int_array_array t =
  let n = ((Array.length t)-1) in
  print_string "[|\n" ; 
  for i = 0 to (n-1) do
    print_int_array t.(i) ; print_string " ;\n"
  done ;
  print_int_array t.(n) ; print_string "\n|]"
;;


let long, haut = 15,15;;

(*
--------------------------------------------------------------------------------------------------------------
|                                                                                                            |
|                                             Matrice de la zone                                             |
|                                                                                                            |
--------------------------------------------------------------------------------------------------------------
*)

(*
  Une case vide       par   0
  Un mur              par   1
  La nourriture       par   2
  La queue du serpent par   3
  La tête du serpent  par   4
*)

let matrice (long,haut) =
  let t = Array.make_matrix haut long 0 in        (*Matrice de la taille voulu rempli 0*)
  t.(haut/2).(long/2) <- 4 ;                      (*La tête du serpent*)
  for i=0 to (long-1) do                          (*Les murs*)
    t.(0).(i)   <- 1 ;
    t.(haut-1).(i) <- 1
  done;
  for i=0 to (haut-1) do                          (*Les murs*)
    t.(i).(0)   <- 1 ;
    t.(i).(long-1) <- 1
  done;
  t, (long,haut);                                 (*On renvoit la zone ainsi que le couple formé par la longueur et la hauteur*)
;;

let zone, (long,haut) = matrice (long,haut);;     (*On recupere la zone ainsi que le couple formé par la longueur et la hauteur*)



(*
--------------------------------------------------------------------------------------------------------------
|                                                                                                            |
|                                             Nourriture aléatoire                                           |
|                                                                                                            |
--------------------------------------------------------------------------------------------------------------
*)

let assiete = ref 0;;                            (*Accumulauteur de la faim*)

let nourrire () =
  if !assiete = 10 then                          (*Cas ou il est tant de nourrire le snake*)
    (
      let () = Random.self_init () in            (*On initialise l'aléatoire*)
      let rec nourriture () =
        let x,y = (Random.int (long-3) + 1), (Random.int (haut-3) + 1) in
        match zone.(x).(y) with
          |0 -> zone.(x).(y) <- 2                (*On remplace la case vide par un repas*)
          |_ -> nourriture ()
      in
      nourriture () ; assiete := 0               (*On réinitialise le compteur*)
    )
;;



(*
--------------------------------------------------------------------------------------------------------------
|                                                                                                            |
|                                             Deplacement du snake                                           |
|                                                                                                            |
--------------------------------------------------------------------------------------------------------------
*)

(*
  0 , (-1,0) -> Direction nord
  1 , (0,1)  -> Direction est
  2 , (1,0)  -> Direction sud
  3 , (0,-1) -> Direction ouest
*)
let direction dir =
  let t = [|(-1,0);(0,1);(1,0);(0,-1)|] in       (*Table de hachage des vecteurs directeurs*)
  t.(dir)
;;

let position () =
  let tete = ref (0,0) in
  for i = 1 to (haut-2) do
    for j = 1 to (long-2) do
      if zone.(i).(j) = 4 then                   (*Dès que la tête est trouvé on place sa position dans une variable*)
        tete := (i,j)
      done
    done;
  !tete
;;                                               (*On renvoit la position de la tête sous forme d'une couple*)

let corps = Queue.create();;
Queue.push (position ()) corps;;

let possible dir (x,y) (xdir,ydir)=              (*On a besoins de la position de la tête*)
  match zone.(x+xdir).(y+ydir) with              (*On verifie que la direction est possible*)
    |0 -> true,false
    |1 -> print_string "GAMEOVER : Vous avez touché un mur !" ; false,false
    |2 -> print_string "Bonus : +1" ; true,true 
    |_ -> print_string "GAMEOVER : Vous vous etes blessé !" ; false,false 
;;                                               (*On revoit un couple de booleen. Le premier indique si le chemin est possible et le second si un bonus de taille doit être ajouté*)

let deplacement dir =
  let x,y = position () in
  let xdir,ydir = direction dir in
  let dep,bonus = possible dir (x,y) (xdir,ydir) in
  if dep then
    (
      Queue.push (x+xdir,y+ydir) corps ;         (*On met dals la file de la queue la case que la tête va quitter*)
      zone.(x).(y) <- 3 ;                        (*On remplace la case que la tête vient de quitter par le corps*)
      zone.(x+xdir).(y+ydir) <- 4 ;              (*On remplace la case dans la direction ou va le snake par la tête*)
      assiete := !assiete + 1 ;                  (*un tour vient de passer, il faut donc ajouter un dans l'accumulateur de la faim*)
      nourrire () ;                              (*Si c'est le tour ou le snake peut etre nourrit, alors un repas apparaitra*)
      if not bonus then                          (*Cas ou le snake n'as pas recu de repas*)
        (
        let xqueue,yqueue = Queue.pop corps in
        zone.(xqueue).(yqueue) <- 0              (*Pour qu'il garde la même taille il faut decaler son corps de un autrement dit supprimer la queue*)
        )
    )
  (*la cas ou le snake ne peut pas se deplacer est déjà dans la fonction "possible"*)
;;



(*
--------------------------------------------------------------------------------------------------------------
|                                                                                                            |
|                                                    Tests                                                   |
|                                                                                                            |
--------------------------------------------------------------------------------------------------------------
*)

(*Nourriture artificielle*)
let x,y = position ();;
zone.(x).(y+2) <- 2;;


(*4 fois a droite, 2 fois en bas, 8 fois a gauche*)
deplacement 1 ; deplacement 1 ; deplacement 1 ; deplacement 1 ;
deplacement 2 ; deplacement 2 ;
deplacement 3 ; deplacement 3 ; deplacement 3 ; deplacement 3 ;
deplacement 3 ; deplacement 3 ; deplacement 3 ; deplacement 3 ;;

(*Tableau final*)
print_int_array_array zone;
Printf.printf "
Caractirisqtiques zone :
long = %2d ; haut = %2d" long haut;;