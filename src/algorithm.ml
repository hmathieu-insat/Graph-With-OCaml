open Graph
open Gfile
open Tools

type path = id list

let big_find_path gr forbidden id1 id2 =
  let rec find_path gr forbidden id1 id2 =
    let arcs = out_arcs gr id1 in
    
      let rec loop arcs forbidden =
        match arcs with 
          | [] -> []
          | (id,_)::reste when (List.exists (fun x->x=id) forbidden) -> loop reste forbidden
          | (id,_)::reste when id=id2 -> [id2]
          | (id,_)::reste -> 
            match (find_path gr (id::forbidden) id id2) with
              | [] -> loop reste forbidden
              | x  -> id::x
      in
    loop arcs forbidden
    in
    id1::(find_path gr forbidden id1 id2)
;;

(* 2 - décrémenter chaque arc du chemin de (flow min)  construire arc retour
4 - ford fulkerson
(se termine quand y'a plus de chemin) *)

let find_min_capa path = match path with
  | [] -> -1
  | hd::tl ->
    let rec loop acu = function
      | [] -> acu
      | hd::tl when hd >= acu -> loop acu tl
      | _ -> loop hd tl
  in
  loop hd path
;;

let rec ret_arcs graph path capa = 
  match path with 
  | [] -> graph
  | _::[] -> graph
  | x1::x2::reste -> 
      let graph1 = add_arc graph x1 x2 (-capa) in (* L'ALLER *)
      let graph2 = add_arc graph1 x2 x1 capa  in (* LE RETOUR *)
      ret_arcs graph2 (x2::reste) capa
;;