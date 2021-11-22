open Graph
open Gfile
open Tools

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

let rec find_path gr forbidden id1 id2 path =
  let arcs = out_arcs gr id1 in
  
    let rec loop arcs forbidden path =
      match arcs with 
        | (id,_) when id=id2 -> Some(id2::path)
        | [] -> forbidden
        | (id,_)::reste -> loop reste (id::forbidden) (id::path)
    in
  
    loop arcs forbidden path
    (* find_path *)
  ;;
    