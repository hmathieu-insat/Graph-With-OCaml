val read_line: string -> acu
type hacker_record  = {id:int;nom:string;litsid:int list}
type lit_record     = {id:int;nom:string;flow:int}
type structure      = {hackers:hacker_record list; lits:lit_record list}

(* Values are read and put in a struct *)
val from_file: path -> structure
