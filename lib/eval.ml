open Expression;;

type value = Const of int | Fun of string * expression * environment
 and variable = { name : string; value : value }
 and environment = { next : environment option;  variable : variable }

type error = VarNotFound of string | TypeError of string
type 'a evaluation = ('a, error) result

let print_error e =
  match e with
  | VarNotFound v -> Printf.printf "Variable not found: %s" v
  | TypeError v -> Printf.printf "Type error: %s" v

let rec find_var name env =
  if env.variable.name = name then
    Ok env.variable.value
  else
    match env.next with
    | Some next -> find_var name next
    | None -> Error (VarNotFound name)

let add_var name value env =
  { next = Some env; variable = { name = name; value = value } }

let rec eval expr env =
  match expr with
  | Var name -> find_var name env
  | Let (name, value, body) ->
     (match (eval value env) with
      | Ok value -> eval body (add_var name value env)
      | Error e -> Error e)
  | Lam (name, _, body) -> Ok (Fun (name, body, env))
  | App (func, arg) ->
     (match eval func env, eval arg env with
      | Ok (Fun (name, body, inner)), Ok arg -> eval body (add_var name arg inner)
      | Ok (Const _), _ -> Error (TypeError "In function application, lhs is not of type fun") 
      | Error e, _ | _, Error e -> Error e)
  | Add (l, r) ->
     (match eval l env, eval r env with
      | Ok (Const lval), Ok (Const rval) -> Ok (Const (lval + rval))
      | Ok (Fun _), _ -> Error (TypeError "In addition, lhs is not an integer")
      | _, Ok (Fun _) -> Error (TypeError "In addition, rhs is not an integer")
      | Error e, _ | _, Error e -> Error e)
  | IntLit value -> Ok (Const value)

