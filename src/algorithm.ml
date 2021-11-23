open Graph
open Gfile
open Tools

type path = id list

(* let find_path gr forbidden id1 id2 =
  let arcs = out_arcs gr id1 in

  let rec loop arcs forbidden path =
    match arcs with 
      | (id,_) when id=id2 -> Some(id2::path)
      | [] -> forbidden
      | (id,_)::reste -> loop reste (id::forbidden) (id::path)
  in

  loop arcs forbidden []
  (* find_path *)
;;
   *)

let rec find_path gr forbidden id1 id2  =
  let arcs = out_arcs gr id1 in
  
    let rec loop arcs forbidden =
      match arcs with 
        | [] -> []
        | (id,_)::reste when (List.exists (fun x->x=id) forbidden) -> loop reste forbidden
        | (id,_)::reste when id=id2 -> [id2]
        | (id,_)::reste -> 
          match (find_path gr forbidden id id2) with
            | [] -> loop reste (id::forbidden)
            | x  -> id::x
    in
  loop arcs forbidden
;;

(* 1 - trouver le plus petit flow d'un arc sur un chemin
2 - décrémenter chaque arc du chemin de (flow min)
3 - construire arc retour
4 - ford fulkerson
(se termine quand y'a plus de chemin) *)

