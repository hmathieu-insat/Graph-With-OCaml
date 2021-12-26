open Graph
open Tools
open Algorithm

type hacker_record  = {idh : int; nomh : string; litsid : int list}
type lit_record     = {idl : int; noml : string; capa : int}
type structure      = {hackers : hacker_record list; lits : lit_record list}

type path = string
let empty_structure = {hackers=[];lits=[]}

(* PRENDS TOUT APRES UN HACKER POUR AVOIR LA LISTE DES LITS *)
let rec read_idlits str acu = 
  let str = String.trim str in
  if str = "" then acu
  else Scanf.sscanf str "%d %s@;" (fun idl reste -> read_idlits reste ((-idl)::acu))

(* PRENDS TOUS LES HACKERS ET LES METS DANS UNE STRUCTURE *)
let read_hacker id structure line =
  try Scanf.sscanf line "h %s@:%s@;" (fun nom args -> 
    { 
      hackers =
      { 
        idh = id;
        nomh = nom;
        litsid = read_idlits args []
      } ::structure.hackers;

      lits = structure.lits
    })
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* PRENDS TOUS LES LITS ET LES METS DANS UNE STRUCTURE *)
let read_lit structure line =
  try Scanf.sscanf line "l %d %s %d" (fun id nom capa -> 
    {
      hackers = structure.hackers;
      lits =
        { 
          idl = (-id);
          noml = nom;
          capa = capa
        } :: structure.lits
    })
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* ON REPREND LE READCOMMENT DU GFILE *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(* PRENDS TOUS LE FICHIER ET LE METS DANS UNE STRUCTURE *)
let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n structure =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, structure2) =
        (* Ignore empty lines *)
        if line = "" then (n, structure)

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'l' -> (n, read_lit structure line)
          | 'h' -> (n+1, read_hacker n structure line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment structure line)
      in      
      loop n2 structure2

    with End_of_file -> structure (* Done *)
  in

  let final_structure = loop 0 empty_structure in

  close_in infile ;
  final_structure

(* S'OCCUPE DES NODES *)
let hacker_node hacker graph = 
  let graph = new_node graph hacker.idh in 
  let graph = add_arc graph 100 hacker.idh 1 in 
  graph

(* S'OCCUPE DES ARCS *)
let rec hacker_arcs idh litsid graph = 
  match litsid with 
  | [] -> graph
  | id::reste -> 
    let graph = add_arc graph idh id 1 in
    let graph = hacker_arcs idh reste graph in
    graph

(* CONVERTIR LISTE DE HACKERS EN NODES + ARCS *)
let rec hackers_to_graph hackers graph =
  match hackers with 
  | [] -> graph
  | hacker::reste -> 
    let graph = hacker_node hacker graph in
    let graph = hacker_arcs hacker.idh hacker.litsid graph in
    let graph = hackers_to_graph reste graph in 
    graph

(* CONVERTIR LISTE DE LITS EN NODES *)
let rec lits_to_graph lits graph = 
  match lits with 
  | [] -> graph
  | lit::reste -> 
    let graph = new_node graph lit.idl in 
    let graph = lits_to_graph reste graph in 
    let graph = add_arc graph lit.idl (-100) lit.capa in
    graph

(* CONVERTIR STRUCTURE EN GRAPH *)
let structure_to_graph structure =
  let graph = empty_graph in 

  let graph = new_node graph 100 in (* Source *)
  let graph = new_node graph (-100) in (* Puit *)

  let graph = lits_to_graph structure.lits graph in 
  let graph = hackers_to_graph structure.hackers graph in
  graph

(* POUR CHAQUE HACKER PRINT OU IL DORT *)
let rec where_sleep hackers lits graph =
  match lits with
  | [] -> ();
  | lit::reste -> let arcs = out_arcs graph lit.idl in
    let rec loop arclist = match arclist with
    | [] -> where_sleep hackers reste graph
    | (destnode, _)::tl when (destnode == 100) -> loop tl
    | (destnode, _)::tl -> Printf.printf "%s dors à %s" (List.nth hackers (destnode - 1)).nomh lit.noml; loop tl
    in loop arcs

(* RESOLUTION TOTALE *)
let solve_hacker path = 
  let structure = from_file path in 
  let graph = structure_to_graph structure in 
  let graph = algo_FF graph 100 (-100) in
  where_sleep structure.hackers structure.lits graph
