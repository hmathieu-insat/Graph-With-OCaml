open Hackparser

let res = read_idlits "1 2 4 8" [];;
List.iter (Printf.printf "%d\n%!") res

(* let res = read_lit empty_structure "l 0 ecole 3";;
let lit = List.nth res.lits 0 ;;
Printf.printf "%d %s %d\n%!" lit.idl lit.noml lit.capa;; *)

(* let res = read_hacker 0 empty_structure "h Hugo:1 2 3 4";;
let hacker = List.nth res.hackers 0 ;;
Printf.printf "%d %s\n%!" hacker.idh hacker.nomh;;
List.iter (Printf.printf "%d\n%!") hacker.litsid;; *)
