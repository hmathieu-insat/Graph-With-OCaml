type hacker_record  = {idh:int;nomh:string;litsid:int list}
type lit_record     = {idl:int;noml:string;capa:int}
type structure      = {hackers:hacker_record list; lits:lit_record list}

let empty_structure = {[];[]}

(* PRENDS TOUS LES HACKERS ET LES METS DANS UNE STRUCTURE *)
let read_hacker id structure line =
  try Scanf.sscanf line "h %s@:%s" (fun nom args -> 
    { 
      hackers =
      { 
        idh = id;
        nomh = nom;
        litsid = (read_line [] args)
      } ::structure.hackers};
      
      lits = structure.lits
    })
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* PRENDS TOUS LES LITS ET LES METS DANS UNE STRUCTURE *)
let read_lit id structure line =
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
          | 'l' -> (n, read_lit n structure line)
          | 'h' -> (n+1, read_hacker structure line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment structure line)
      in      
      loop n2 structure2

    with End_of_file -> structure (* Done *)
  in

  let final_structure = loop 0 empty_structure in

  close_in infile ;
  final_structure