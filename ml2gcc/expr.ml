let line = ref 0

type name = string

type data_type =
	Int
  | Cons of data_type * data_type
  | Fun of (data_type list) * data_type
  | Unknown

type pos = int

type 'a expr =
  | EConst of int
  | EAdd of ('a * 'a expr) * ('a * 'a expr)
  | ESub of ('a * 'a expr) * ('a * 'a expr)
  | EMul of ('a * 'a expr) * ('a * 'a expr)
  | EDiv of ('a * 'a expr) * ('a * 'a expr)
  | EEq of ('a * 'a expr) * ('a * 'a expr)
  | EGt of ('a * 'a expr) * ('a * 'a expr)
  | EGte of ('a * 'a expr) * ('a * 'a expr)
  | ECar of ('a * 'a expr)
  | ECdr of ('a * 'a expr)
  | EAtom of ('a * 'a expr)
  | EIf of ('a * 'a expr) * ('a * 'a expr) * ('a * 'a expr)
  | ELetIn of ((name * ('a * 'a expr)) list) * ('a * 'a expr)
  | ERecIn of ((name * ('a * 'a expr)) list) * ('a * 'a expr)
  | EMatch of ('a * 'a expr) * ((pattern * ('a * 'a expr)) list)
  | EVar of name
  | ECons of ('a * 'a expr) * ('a * 'a expr)
  | EFun of (name list) * ('a * 'a expr)
  | EApp of ('a * 'a expr) * (('a * 'a expr) list)
  | EDebug of ('a * 'a expr)

and pattern =
  | PAny
  | PVar of name
  | PConst of int
  | PCons of pattern * pattern

let string_of_pos p = "[line:" ^ string_of_int p ^ "]"
