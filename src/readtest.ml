open Hackparser

(* Test fonction read_idlits *)

(* let res = read_idlits "1 2 3;" [];;
List.iter (Printf.printf "%d\n%!") res

(* Test fonction read_lit *)

let res = read_lit empty_structure "l 0 ecole 3";;
let lit = List.nth res.lits 0 ;;
Printf.printf "%d %s %d\n%!" lit.idl lit.noml lit.capa;;

(* Test fonction read_hacker *)

let res = read_hacker 0 empty_structure "h Hugo:1 2 3 4;";;
let hacker = List.nth res.hackers 0 ;;
Printf.printf "%d %s\n%!" hacker.idh hacker.nomh;;
List.iter (Printf.printf "%d\n%!") hacker.litsid;; *)

(* Test global de from_file *)

(* let res = from_file "graphs/hackersleep";;

let id = 1;;

let lit = List.nth res.lits id ;;
Printf.printf "%d %s %d\n%!" lit.idl lit.noml lit.capa

let hacker = List.nth res.hackers id ;;
Printf.printf "%d %s\n%!" hacker.idh hacker.nomh;;
List.iter (Printf.printf "%d\n%!") hacker.litsid;; *)
