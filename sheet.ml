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

let rec insert_in_reperc_list_without_duplicates co list = match list with
  | [] -> [co]
  | t::q when ((fst co) < (fst t)) || (((fst co) = (fst t)) && ((snd co) < (snd t))) -> co::list
  | t::q when ((fst co) = (fst t)) && ((snd co) = (snd t)) -> list
  | t::q -> t::(insert_in_reperc_list_without_duplicates co q)



(* initialisation du tableau : questions un peu subtiles de partage,
 * demandez autour de vous si vous ne comprenez pas pourquoi cela est
 * nécessaire.  
 * Vous pouvez ne pas appeler la fonction ci-dessous,
 * modifier une case du tableau à l'aide de update_cell_formula, et
 * regarder ce que ça donne sur le tableau : cela devrait vous donner
 * une piste *)
let init_sheet () =
  let init_cell i j =
    let c = { value = None; formula = Cst 0.; repercussions = []; error = false } in
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
      print_string (cell_val2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()





(********** calculer les valeurs à partir des formules *************)

(* On crée une fonction qui a pour rôle de faire propager une erreur de dépendance dans toutes une liste de cellule. Si une cellule C dépend d'une autre cellule C', et que cette cellule C' dépend encore d'une autre cellule C'', alors la construction des listes de repercussions est faite de telle sorte que la cellule C apparaisse dans la liste de repercussions de C''. Il n'y a donc pas besoin de réinjecter les listes de repercussions à chaque nouvelle cellule rencontrée : la liste de départ contient toutes les autres *)
let rec corrupt_cell_repercussions repercs_list = match repercs_list with
  | [] -> ()
  | (i,j)::q ->
     begin
       thesheet.(i).(j).error <- true;
       corrupt_cell_repercussions q
     end


(* à faire : le cœur du programme *)    
let rec eval_form_from_cell_call reperc_list fo i j= match fo with
  | Cst(n) ->
     begin
       thesheet.(i).(j).repercussions <- reperc_list;
       n
     end
  | Cell(p,q) -> eval_cell reperc_list p q
  | Op(o,fs) -> eval_op reperc_list fo

and eval_form_from_form_call reperc_list fo = match fo with
  | Cst(n) -> n
  | Cell(p,q) -> eval_cell reperc_list p q
  | Op(o,fs) -> eval_op reperc_list fo

and eval_op reperc_list fo = match fo with
  | Op(S,[]) -> failwith "somme vide"
  | Op(S,[t]) -> eval_form_from_form_call reperc_list t
  | Op(S,t::q) -> (eval_form_from_form_call reperc_list t)+.(eval_op reperc_list (Op(S,q)))
  | Op(M,[]) -> failwith "produit vide"
  | Op(M,[t]) -> eval_form_from_form_call reperc_list t
  | Op(M,t::q) -> (eval_form_from_form_call reperc_list t)*.(eval_op reperc_list (Op(M,q)))
  | Op(A,[]) -> failwith "moyenne vide"
  | Op(A,[t]) -> eval_form_from_form_call reperc_list t
  | Op(A,l) -> (eval_form_from_form_call reperc_list (Op(S,l)))/.(float_of_int (List.length l))
  | Op(MAX,[]) -> failwith "max vide"
  | Op(MAX,[t]) -> eval_form_from_form_call reperc_list t
  | Op(MAX,t::q) -> max (eval_form_from_form_call reperc_list t) (eval_op reperc_list (Op(MAX,q)))
  | _ -> failwith "c'est mauvais !"

and eval_cell reperc_list i j =
  if ((List.mem (i,j) reperc_list) || (thesheet.(i).(j).error = true))
  then
    (* Si on apparait dans la liste de nos repercussions, c'est qu'il y a une boucle dans les dépendances. Il peut aussi déjà y avoir une erreur de dépendance dans notre cellule. Dans tous les cas, on appelle une fonction auxilière qui va s'occuper de faire disjoncter les cellules qui nous ont été envoyées dans reperc_list (c'est-à-dire y compris notre cellule), puisque ces même cellules dépendant de notre cellule qui est buguée. On garde quand même cette liste de repercussions pour que si plus tard la formule de la cellule est modifiée pour qu'il n'y ait plus de problème, alors on pourra accéder facilement aux cellules anciennement repercutées *)
    begin
      corrupt_cell_repercussions reperc_list;
      0. (* Je décide de renvoyer 0 comme je pourrais renvoyer n'importe quoi, puisque cette cellule et celles qui dépendent d'elle ont été mises sous erreur de dépendance. Si on voudrait faire les choses proprement, il faudrait même renvoyer un type d'erreur spécifique aux valeurs, qui ferait comme dans l'exam de ThProg "BOOM", mais serait trop lours à implémenter *)
    end
  else
    begin
      (* on injecte les coordonnées de notre cellule courante dans une liste qui va être ajoutée aux listes des repercussions des cellules apparaissant dans les dépendances de la cellule courante *)
      let evaluation = eval_form_from_cell_call (insert_in_reperc_list_without_duplicates (i,j) reperc_list) ((read_cell (i,j)).formula) i j in
      thesheet.(i).(j).value <- Some(evaluation);
      evaluation
    end
  

(* on marque qu'on doit tout recalculer en remplissant le tableau de "None" et puis on recalcule*)
let invalidate_sheet () = 
  let init_value i j =
    thesheet.(i).(j).value <- None
  in

  sheet_iter init_value


(* on recalcule le tableau, en deux étapes *)
let recompute_sheet () =
  invalidate_sheet ();
  sheet_iter (eval_cell [])


let update_cell_formula co f = 
  thesheet.(fst co).(snd co).formula <- f;
  recompute_sheet ()


let update_cell_value co v =
  thesheet.(fst co).(snd co).value <- v


