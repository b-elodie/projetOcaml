open Debug
open Cell
open Sheet



(* commandes: ce que l'utilisateur peut saisir dans un fichier.
 - La modification d'une cellule avec une nouvelle formule,
 - l'affichage d'une cellule, 
 - l'affichage de toute la feuille *)
type comm = Upd of cellname * form | Show of cellname | ShowAll | ShowVal of cellname | ShowAllVal | ShowError of cellname | ShowAllError



(************ affichage **************)

let show_comm c =
  match c with
  | Upd (c,f) ->
     begin
       ps (cell_name2string c);
       ps"=";
       show_form f
     end
  | Show c ->
     begin
       ps "Show(";
       ps (cell_name2string c);
       ps ")"
     end
  | ShowAll -> ps "ShowAll"
  | ShowVal c ->
     begin
       ps "ShowVal(";
       ps (cell_name2string c);
       ps ")"
     end
  | ShowAllVal -> ps "ShowAllVal"
  | ShowError c ->
     begin
       ps "ShowError(";
       ps (cell_name2string c);
       ps ")"
     end
  | ShowAllError -> ps "ShowAllError"



(************ faire tourner les commandes **************)

(* exécuter une commande *)

let run_command c = match c with
  | Show cn ->
     begin
       let co = cellname_to_coord cn in
       eval_p_debug (fun () -> "Showing cell " ^ cell_name2string cn);
       ps (cell_content2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
       print_newline()
     end
  | ShowAll ->
     begin
       eval_p_debug (fun () -> "Show All\n");
       show_sheet ()
     end
  | ShowVal cn ->
     begin
       let co = cellname_to_coord cn in
       eval_p_debug (fun () -> "Showing value of cell " ^ cell_name2string cn);
       ps (cell_val2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
       print_newline()
     end
  | ShowAllVal ->
     begin
       eval_p_debug (fun () -> "Show All Val\n");
       show_sheet_val ()
     end
  | ShowError cn ->
     begin
       let co = cellname_to_coord cn in
       eval_p_debug (fun () -> "Showing error state of cell " ^ cell_name2string cn);
       ps (cell_error2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
       print_newline()
     end
  | ShowAllError ->
     begin
       eval_p_debug (fun () -> "Show All Error\n");
       show_sheet_error ()
     end
  | Upd(cn,f) ->
     let co = cellname_to_coord cn in
     eval_p_debug (fun () -> "Update cell " ^ cell_name2string cn ^ "\n");
     update_cell_formula co f

(* exécuter une liste de commandes *)
let run_script cs = List.iter run_command cs
