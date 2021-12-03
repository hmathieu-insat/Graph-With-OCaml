type hacker_record  = {idh : int; nomh : string; litsid : int list}
type lit_record     = {idl : int; noml : string; capa : int}
type structure      = {hackers : hacker_record list; lits : lit_record list}

(* Values are read and put in a struct *)
val from_file: path -> structure

val read_line: string -> acu