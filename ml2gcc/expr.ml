type name = string

type value = int

and expr =
  | EConst of value
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EEq of expr * expr
  | EGt of expr * expr
  | EGte of expr * expr
  | ECar of expr
  | ECdr of expr
  | EAtom of expr
  | EIf of expr * expr * expr
  | ELetIn of ((name * expr) list) * expr
  | ERecIn of ((name * expr) list) * expr
  | EMatch of expr * ((pattern * expr) list)
  | EVar of name
  | ECons of expr * expr
  | EFun of (name list) * expr
  | EApp of expr * (expr list)

and pattern =
  | PAny
  | PVar of name
  | PConst of value
  | PCons of pattern * pattern
