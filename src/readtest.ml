open Hackparser

(* let res = read_idlits "1 2 4 8" [];;
List.iter (Printf.printf "%d\n%!") res *)

let res = read_lit empty_structure "l 0 ecole 3";;
let lit = List.nth res.lits 0 ;;
Printf.printf "%d %s %d\n!" lit.idl lit.noml lit.capa;;