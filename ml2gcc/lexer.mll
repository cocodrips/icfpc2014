{
  open Expr
  open Parser
  exception Parse_error
}

let digit = ['0' - '9']
let small = ['a' - 'z']
let alpha = ['a' - 'z' 'A' - 'Z' '0' - '9' '_']

rule token = parse
  | "(*"        { comments 0 lexbuf }
  | [' ' '\t']* { token lexbuf }
  | '\n'        { token lexbuf }
  | digit+ as n { INT (int_of_string n) }
  | "let"       { LET }
  | "in"        { IN }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "&&"        { AND2 }
  | "||"        { OR2 }
  | "not"       { NOT }
  | "car"       { CAR }
  | "cdr"       { CDR }
  | "atom"      { ATOM }
  | "fun"       { FUN }
  | "rec"       { REC }
  | "and"       { AND }
  | "match"     { MATCH }
  | "with"      { WITH }
  | ","         { COMMA }
  | ";"         { SEMI }
  | "|"         { OR }
  | "->"        { ARROW }
  | ">="        { GTE }
  | '>'         { GT }
  | '='         { EQ }
  | "<="        { LTE }
  | '<'         { LT }
  | "!="        { NEQ }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIV }
  | '('         { LPAR }
  | ')'         { RPAR }
  | '['         { LLIST }
  | ']'         { RLIST }
  | '_'         { UNDER }
  | "::"        { CONS }
  | "true"      { INT 1 }
  | "false"     { INT 0 }
  | small(alpha*) as id { ID id }
  | eof         { EOF }
  | _           { raise Parse_error }

and comments depth = parse
  | "*)"
	  {
		if depth = 0 then
		  token lexbuf
		else
		  comments (depth-1) lexbuf
	  }
  | "(*"	{ comments (depth+1) lexbuf }
  | eof		{ raise Parse_error }
  | '\n'    { comments depth lexbuf }
  | _		{ comments depth lexbuf }
