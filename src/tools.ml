(* Yes, we have to repeat open Graph. *)
open Graph

let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph;;
let gmap gr f = assert false;;
let add_arc g id1 id2 n = assert false;;
