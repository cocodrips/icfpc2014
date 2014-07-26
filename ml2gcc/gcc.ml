type gcc =
  |	GLabel of string
  | GLdc of int
  | GLd of int * int
  | GLdf of string
  | GAp of int
  | GAdd
  | GSub
  | GMul
  | GDiv
  | GRtn
  | GDbug
  | GCeq
  | GCgt
  | GCgte
  | GAtom
  | GCons
  | GCar
  | GCdr
  | GSel of string * string
  | GJoin
  | GDum of int
  | GRap of int
  | GStop
  | GTsel of string * string
  | GTap of int
  | GTrap of int
  | GSt of int * int
  | GBrk

let string_of_gcc =
  let str = string_of_int in function
	| GLabel s -> ":" ^ s
	| GLdc i -> "LDC " ^ str i
	| GLd (n, i) -> "LD " ^ str n ^ " " ^ str i
	| GLdf label -> "LDF @" ^ label
	| GAdd -> "ADD"
	| GSub -> "SUB"
	| GMul -> "MUL"
	| GDiv -> "DIV"
	| GRtn -> "RTN"
	| GDbug -> "DBUG"
	| GCeq -> "CEQ"
	| GCgt -> "CGT"
	| GCgte -> "CGTE"
	| GAp n -> "AP " ^ str n
	| GAtom -> "ATOM"
	| GCons -> "CONS"
	| GCar -> "CAR"
	| GCdr -> "CDR"
	| GSel (t,f) -> "SEL @" ^ t ^ " @" ^ f
	| GJoin -> "JOIN"
	| GDum n -> "DIM " ^ str n
	| GRap n -> "RAP " ^ str n
	| GStop -> "STOP"
	| GTsel (t, f) -> "TSEL @" ^ t ^ " @" ^ f
	| GTap n -> "TAP " ^ str n
	| GTrap n -> "TRAP" ^ str n
	| GSt (n, i) -> "ST " ^ str n ^ " " ^ str i
	| GBrk -> "BRK"

let rec print_gccs = function
  | [] -> ()
  | x::xs -> (
	print_string (string_of_gcc x);
	print_newline ();
	print_gccs xs)
