type expression =
  | Var of string
  | Add of expression * expression
  | Let of string * expression * expression
  | Lam of string * expression
  | App of expression * expression
  | IntLit of int
