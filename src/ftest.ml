open Gfile
open Tools
open Algorithm
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    -  infile  : input file containing a graph\n" ^
         "    -  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    -  sink    : identifier of the sink vertex (ditto)\n" ^
         "    -  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Tests *)
  (* let graph2 = clone_nodes graph in *)
  let graph = gmap graph (fun x-> int_of_string x) in
  let graph = add_arc graph 2 4 9999 in


  let chemin = find_path graph [1;2] 0 5 in
  List.iter (Printf.printf "%d\n%!") chemin;

  
  (* Rewrite the graph that has been read. *)
  let graph = gmap graph (fun x-> string_of_int x) in
  (* let () = export outfile graph in *)
  let () = write_file outfile graph in

  ()

