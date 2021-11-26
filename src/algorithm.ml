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
      | (_,0)::reste -> loop reste forbidden
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

let find_min_capa graph path = 
  match path with
  | [] -> 0
  | _::[] -> 0
  | hd::tl -> let rec loop acu = function
      | [] -> -1 (* Unnecessary pattern-matching here, written solely to please ocaml compilator *) 
      | _::[] -> acu
      | x1::x2::tl -> begin match (find_arc graph x1 x2) with
          | Some x -> if x >= acu then loop acu (x2::tl) else loop x (x2::tl)
          | None -> raise Not_found
        end
    in
    loop max_int path


let rec ret_arcs graph path capa = 
  match path with 
  | [] -> graph
  | _::[] -> graph
  | x1::x2::reste -> 
    let graph1 = add_arc graph x1 x2 (-capa) in (* L'ALLER *)
    let graph2 = add_arc graph1 x2 x1 capa  in (* LE RETOUR *)
    ret_arcs graph2 (x2::reste) capa


let rec algo_FF graph s p =
  let path = big_find_path graph [] s p in
  let capa = find_min_capa graph path in
  match capa with
  | 0 -> graph
  | _ -> algo_FF (ret_arcs graph path capa) s p
  