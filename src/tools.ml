(* Yes, we have to repeat open Graph. *)
open Graph

let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph;;

let gmap gr f = 
  let g acu id1 id2 lbl = new_arc acu id1 id2 (f lbl) in
  e_fold gr g (clone_nodes gr);;

let add_arc gr id1 id2 n = 
  match find_arc gr id1 id2 with
  | Some x -> new_arc gr id1 id2 (x+n)
  | None -> new_arc gr id1 id2 n
;;
