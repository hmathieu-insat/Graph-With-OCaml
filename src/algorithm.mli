open Graph
open Gfile
open Tools

(* A path is a list of nodes. *)
type path = id list

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
*)
val big_find_path: int graph -> id list -> id -> id -> path

val find_min_capa: int graph -> path -> int

val ret_arcs: int graph -> path -> int -> int graph

val algo_FF: int graph -> id -> id -> int graph