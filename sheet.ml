(* tableau de cellules *)
open Cell

let size = (20,10) (* lignes, colonnes *)

(* le tableau que l'on manipule dans le programme ; *)
(* si nécessaire, tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *) 
(* default_cell est défini dans cell.ml (module Cell) *)
let thesheet = Array.make_matrix (fst size) (snd size) default_cell

let read_cell co = thesheet.(fst co).(snd co)


(* exécuter une fonction, f, sur tout le tableau *)
let sheet_iter f =
  for i = 0 to (fst size -1) do
    for j = 0 to (snd size -1) do
      f i j
    done;
  done

(* exécuter une fonction, f, sur une liste de coordonnées de cellules *)
let rec sheet_iter_over_coos_list f coos_list = match coos_list with
  | [] -> ()
  | t::q -> f (fst t) (snd t);
            sheet_iter_over_coos_list f q


let rec insert_in_coos_list_without_duplicates co list = match list with
  | [] -> [co]
  | t::q when ((fst co) < (fst t)) || (((fst co) = (fst t)) && ((snd co) < (snd t))) -> co::list
  | t::q when ((fst co) = (fst t)) && ((snd co) = (snd t)) -> list
  | t::q -> t::(insert_in_coos_list_without_duplicates co q)

let rec union_of_coos_lists_without_duplicates list1 list2 = match list1,list2 with
  | _,[] -> list1
  | [],_ -> list2
  | t::q,_ -> union_of_coos_lists_without_duplicates q (insert_in_coos_list_without_duplicates t list2)

let rec delete_in_list co list = match list with
  | [] -> []
  | t::q when t = co -> q (* la liste de repercussion est sans doublon par construction *)
  | t::q -> t::(delete_in_list co q)

let rec print_co_list co_list = match co_list with
  | [] -> print_string "$\n";
  | t::q -> print_string ("(" ^ (string_of_int (fst t)) ^ ";" ^ (string_of_int (snd t)) ^ ") ");
            print_co_list q



(* initialisation du tableau : questions un peu subtiles de partage,
 * demandez autour de vous si vous ne comprenez pas pourquoi cela est
 * nécessaire.  
 * Vous pouvez ne pas appeler la fonction ci-dessous,
 * modifier une case du tableau à l'aide de update_cell_formula, et
 * regarder ce que ça donne sur le tableau : cela devrait vous donner
 * une piste *)
let init_sheet () =
  let init_cell i j =
    let c = { value = None; formula = Cst 0.; dependencies = []; repercussions = []; error = false } in
    thesheet.(i).(j) <- c
  in
  sheet_iter init_cell

(* on y va, on initialise *)
let _ = init_sheet ()


(* affichage rudimentaire du tableau *)

let show_sheet () =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
      print_string (cell_content2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()

let show_sheet_val () =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
      print_string (cell_val2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()

let show_sheet_error () =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
      print_string (cell_error2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()



(********** calculer les valeurs à partir des formules *************)

(* On crée une fonction qui a pour rôle de faire propager une erreur de dépendance dans toutes une liste de cellule. Si une cellule C dépend d'une autre cellule C', et que cette cellule C' dépend encore d'une autre cellule C'', alors il faut aussi faire buguer C''. On ajoute donc la liste de repercussion de C' à la liste des cellules à faire buguer. On comprend donc le schéma : à chaque nouvelle cellule rencontrée, SI elle n'est pas encore buguée, on la fait buguée et on propage aussi à sa liste de repercussion *)
let rec corrupt_cell_repercussions repercs_list = match repercs_list with
  | [] -> ()
  | (i,j)::q ->
     if not (thesheet.(i).(j).error)
     then
       begin
         thesheet.(i).(j).error <- true;
         corrupt_cell_repercussions q;
         corrupt_cell_repercussions (thesheet.(i).(j).repercussions);
       end

(* On crée une fonction qui prend une liste de repercussion complète, et qui se propage depuis une liste de dépendances dans toutes les dépendances pour regarder si on a pas une erreur de dépendance*)
let rec test_repercussion_error full_repercs_list deps_list = match deps_list with
  | [] -> ()
  | t::q when ((List.mem t full_repercs_list) || (thesheet.(fst t).(snd t).error = true)) ->
    (* Si on se repercute soi-même, c'est qu'il y a une boucle dans les dépendances. Il peut aussi déjà y avoir une erreur de dépendance dans notre cellule, du fait qu'une cellule dont on dépend a elle même une erreur de dépendance. Dans tous les cas, on appelle une fonction auxilière qui va s'occuper de faire disjoncter les cellules qui nous ont été envoyées dans repercs_list (c'est-à-dire après plusieurs étapes y compris notre propre cellule), puisque ces même cellules dépendant de notre cellule qui est buguée. On garde quand même la liste de repercussion qui nous a été donnée pour que si plus tard la formule de la cellule est modifiée de manière à ce qu'il n'y ait plus de problème, alors on pourra accéder facilement aux cellules anciennement repercutées pour les actualiser *)
    corrupt_cell_repercussions full_repercs_list
  | t::q -> test_repercussion_error (t::full_repercs_list) thesheet.(fst t).(snd t).dependencies;
            test_repercussion_error full_repercs_list q



(********** à faire : le cœur du programme **********)    

(* fonction qui renvoie un couple avec en premier la valeur de la formule donnée évaluée, et en deuxième la liste des cellules dont dépend cette formule avec un seul niveau de profondeur en dépendance *)
let rec eval_form repercs_list full_repercs_list fo = match fo with
  | Cst(n) -> (n, [])
  | Cell(p,q) -> (eval_cell repercs_list full_repercs_list p q, [(p,q)])
  | Op(o,fs) -> eval_op repercs_list full_repercs_list fo

and eval_op repercs_list full_repercs_list fo = match fo with
  | Op(S,[]) -> failwith "somme vide"
  | Op(S,[t]) -> eval_form repercs_list full_repercs_list t
  | Op(S,t::q) -> let (val1, deps_list1) = eval_form repercs_list full_repercs_list t in
                  let (val2, deps_list2) = eval_op repercs_list full_repercs_list (Op(S,q)) in
                  (val1+.val2, union_of_coos_lists_without_duplicates deps_list1 deps_list2) (* on fait une union sans doublons pour n'avoir que [A1] comme liste de dépendance pour la formule A1+A1+A1 *)
  | Op(M,[]) -> failwith "produit vide"
  | Op(M,[t]) -> eval_form repercs_list full_repercs_list t
  | Op(M,t::q) -> let (val1, deps_list1) = eval_form repercs_list full_repercs_list t in
                  let (val2, deps_list2) = eval_op repercs_list full_repercs_list (Op(M,q)) in
                  (val1*.val2, union_of_coos_lists_without_duplicates deps_list1 deps_list2) (* idem que pour la somme mais avec le produit *)
  | Op(A,[]) -> failwith "moyenne vide"
  | Op(A,[t]) -> eval_form repercs_list full_repercs_list t
  | Op(A,l) -> let (val1, deps_list1) = eval_form repercs_list full_repercs_list (Op(S,l)) in
               (val1/.(float_of_int (List.length l)), deps_list1)
  | Op(MAX,[]) -> failwith "max vide"
  | Op(MAX,[t]) -> eval_form repercs_list full_repercs_list t
  | Op(MAX,t::q) -> let (val1, deps_list1) = eval_form repercs_list full_repercs_list t in
                    let (val2, deps_list2) = eval_op repercs_list full_repercs_list (Op(MAX,q)) in
                    (max val1 val2, union_of_coos_lists_without_duplicates deps_list1 deps_list2)
  | _ -> failwith "c'est mauvais !"

and eval_cell repercs_list full_repercs_list i j = (* repercs_list est la liste de repercussion à un seul niveau de prondeur "en avant" de la cellule envoyée, et full_repercs_list est la liste complète de toutes les cellules que l'on repércute, qui est utilisée ici pour savoir si on a une boucle dans les dépendances *)
  if ((List.mem (i,j) full_repercs_list) || (thesheet.(i).(j).error = true))
  then
    (* Si on se repercute soi-même, c'est qu'il y a une boucle dans les dépendances. Il peut aussi déjà y avoir une erreur de dépendance dans notre cellule, du fait qu'une cellule dont on dépend a elle même une erreur de dépendance. Dans tous les cas, on appelle une fonction auxilière qui va s'occuper de faire disjoncter les cellules qui nous ont été envoyées dans repercs_list (c'est-à-dire après plusieurs étapes y compris notre propre cellule), puisque ces même cellules dépendant de notre cellule qui est buguée. On garde quand même la liste de repercussion qui nous a été donnée pour que si plus tard la formule de la cellule est modifiée de manière à ce qu'il n'y ait plus de problème, alors on pourra accéder facilement aux cellules anciennement repercutées pour les actualiser *)
    begin
      thesheet.(i).(j).repercussions <- (union_of_coos_lists_without_duplicates (thesheet.(i).(j).repercussions) repercs_list);
      corrupt_cell_repercussions repercs_list;
      (*print_string ("-> Evaluation de la cellule (" ^ (string_of_int i) ^ ";" ^ (string_of_int j) ^ ") en 0.\n");
      print_string "listes des répercussion qu'il faudra actualiser :\n";
      print_co_list thesheet.(i).(j).repercussions;*)
      thesheet.(i).(j).value <- Some(0.);
      0. (* Je décide de renvoyer 0 comme je pourrais renvoyer n'importe quoi, puisque cette cellule et celles qui dépendent d'elle ont été mises sous erreur de dépendance. Si on voudrait faire les choses proprement, il faudrait même renvoyer un type d'erreur spécifique aux valeurs, qui ferait comme dans l'exam de ThProg "BOOM", mais serait trop lours à implémenter *)
    end
  else
    match thesheet.(i).(j).value with
    | None ->
      (* on injecte les coordonnées de notre cellule courante dans une liste qui va être ajoutée aux listes des repercussions des cellules apparaissant dans les dépendances de la cellule courante *)
      let (val1, deps_list1) = eval_form [(i,j)] ((i,j)::full_repercs_list) ((read_cell (i,j)).formula) in (* comme on évalue une nouvelle cellule, on remet à 0 sa liste de repercussion à un seul niveau de profondeur "en avant". Pour sa liste de repercussion complète, comme on peut supposer que (i,j) n'apparait pas déjà dedans sinon on aurait une erreur de dépendance et on tomberait dans le premier de notre fonction, alors on peut directement ajouter le couple (i,j) dans full_repercs_list sans risquer de doublon *)
      thesheet.(i).(j).value <- Some(val1);
      thesheet.(i).(j).dependencies <- deps_list1;
      thesheet.(i).(j).repercussions <- (union_of_coos_lists_without_duplicates (thesheet.(i).(j).repercussions) repercs_list);
      (*print_string ("-> Evaluation de la cellule (" ^ (string_of_int i) ^ ";" ^ (string_of_int j) ^ ") en " ^ (string_of_float val1) ^ "\n");
      print_string "listes des répercussion qu'il faudra actualiser :\n";
      print_co_list thesheet.(i).(j).repercussions;*)
      val1
    | Some(val1) ->
      (* Si on a déjà une valeur, c'est qu'on est à jour pour la valeur. Cependant, on peut ne pas être à jour pour une erreur de dépendance : si une cellule C dépend d'une cellule C', et que C' dépend d'une celulle C'' tout ça sans pb de dépendance, mais qu'ensuite on change la formule de C'' de sorte qu'elle dépende en plus de C, alors il ne faut pas arrêter l'exploration des repercussions à C même si C a déjà une valeur : on va continuer l'exploration mais juste pour regarder les erreurs de dépendances, sans toucher aux valeurs, et tout ça grâce à une fonction auxilière. Cette fonction n'aura pas à toucher aux listes de repercussion puisqu'elles ne sont que de profondeur 1 *)
      thesheet.(i).(j).repercussions <- (union_of_coos_lists_without_duplicates (thesheet.(i).(j).repercussions) repercs_list);
      test_repercussion_error full_repercs_list thesheet.(i).(j).dependencies;
      (*print_string ("-> Evaluation de la cellule (" ^ (string_of_int i) ^ ";" ^ (string_of_int j) ^ ") en " ^ (string_of_float val1) ^ "\n");
      print_string "listes des répercussion qu'il faudra actualiser :\n";
      print_co_list thesheet.(i).(j).repercussions;*)
      val1
  

(* on marque qu'on doit tout recalculer en remplissant le tableau de "None" et puis on recalcule*)
let invalidate_sheet () = 
  let init_value i j =
    thesheet.(i).(j).value <- None
  in

  sheet_iter init_value

let rec invalidate_repercs_list repercs_list = match repercs_list with
  | [] -> ()
  | t::q when thesheet.(fst t).(snd t).value <> None ->
     (*print_string ("-> Invalidation de la cellule (" ^ (string_of_int (fst t)) ^ ";" ^ (string_of_int (snd t)) ^ ")\n");*)
     thesheet.(fst t).(snd t).value <- None;
     thesheet.(fst t).(snd t).error <- false;
     (*print_string "Liste de répercussion de t à invalider :\n";
     print_co_list thesheet.(fst t).(snd t).repercussions;*)
     invalidate_repercs_list thesheet.(fst t).(snd t).repercussions;
     (*print_string ("Liste q de (" ^ (string_of_int (fst t)) ^ ";" ^ (string_of_int (snd t)) ^ ") à invalider :\n");
     print_co_list q;*)
     invalidate_repercs_list q
  | t::q ->
     (*print_string ("-> Pas d'invalidation à faire pour (" ^ (string_of_int (fst t)) ^ ";" ^ (string_of_int (snd t)) ^ ")\n");
     print_string ("Liste q de (" ^ (string_of_int (fst t)) ^ ";" ^ (string_of_int (snd t)) ^ ") à invalider :\n");
     print_co_list q;*)
     invalidate_repercs_list q

let rec eval_repercs_lists repercs_list = (*print_string "eval_repercs_lists call";*)
  match repercs_list with
  | [] -> ()
  | t::q when thesheet.(fst t).(snd t).value = None ->
     let _ = eval_cell [] [] (fst t) (snd t) in
     eval_repercs_lists q;
     eval_repercs_lists thesheet.(fst t).(snd t).repercussions
  | t::q -> eval_repercs_lists q

            
(* on recalcule le tableau, en deux étapes *)
(*let recompute_sheet () =
  print_string "-> Recalculaaage de toute la table\n";
  invalidate_sheet ();
  sheet_iter (eval_cell [] [])*)

let rec delete_from_deps_list co deps_list = match deps_list with
  | [] -> ()
  | (i,j)::q ->
     thesheet.(i).(j).repercussions <- (delete_in_list co (thesheet.(i).(j).repercussions));
     delete_from_deps_list co q


let update_cell_formula co f =
  (*print_string ("-> Mise à jour de la formule de la cellule (" ^ (string_of_int (fst co)) ^ ";" ^ (string_of_int (snd co)) ^ ") avec " ^ (form2string f) ^ "\n");*)
  (* On commence par mettre à jour la formule de la cellule *)
  thesheet.(fst co).(snd co).formula <- f;

  (* On invalide ensuite la valeur de la cellule, et on repasse la cellule en mode sans erreur (qui pourra après calcul revenir en mode erreur) *)
  thesheet.(fst co).(snd co).value <- None;
  thesheet.(fst co).(snd co).error <- false;
  (* on se supprime tout de suite des listes de repercussions des cellules dont on dépend *)
  delete_from_deps_list co (thesheet.(fst co).(snd co).dependencies);
  (* on enchaîne en recalculant la valeur de la cellule avec sa formule toute fraîche. Cela a pour effet d'en même temps se rajouter dans les listes de repercussion de ses nouvelles cellules dont elle dépend, et de regarder si on a pas créer ou recréer une erreur de dépendance *)
  let _ = eval_cell (thesheet.(fst co).(snd co).repercussions) (thesheet.(fst co).(snd co).repercussions) (fst co) (snd co) in (* même si on change la formule d'une cellule, sa liste de repercussion ne change pas *)
  (* puis on actualise toute les VALEURS des cellules de sa liste de repercussion après les avoir invalidées *)
  (*print_string "listes des répercussion à actualiser :\n";
  print_co_list thesheet.(fst co).(snd co).repercussions;*)
  invalidate_repercs_list thesheet.(fst co).(snd co).repercussions;
  print_string "Fin d'invalidation, et évaluation des repércussions\n";
  print_co_list thesheet.(fst co).(snd co).repercussions;
  (*sheet_iter_over_coos_list (eval_cell [] []) thesheet.(fst co).(snd co).repercussions c'est faux*)
  eval_repercs_lists thesheet.(fst co).(snd co).repercussions

(*let update_cell_value co v =
  print_string ("-> Mise à jour de la valeur de la cellule (" ^ (string_of_int (fst co)) ^ ";" ^ (string_of_int (snd co)) ^ ")\n");
  thesheet.(fst co).(snd co).value <- v*)

