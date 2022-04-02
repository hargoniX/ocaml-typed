open Expression;;
open Eval;;

module TypeMap = Map.Make(String)

type typing = typ TypeMap.t

let rec infer_type expr typing =
  match expr with
  | IntLit _ -> Ok Integer
  | Var name ->
     (match TypeMap.find_opt name typing with
      | Some typ -> Ok typ
      | None -> Error (VarNotFound name))
  | Add (l, r) ->
     (match infer_type l typing, infer_type r typing with
      | Ok Integer, Ok Integer -> Ok Integer
      | Ok (Function _), _ -> Error (TypeError "Type error in addition, lhs is not an integer")
      | _, Ok (Function _) -> Error (TypeError "Type error in addition, rhs is not an integer")
      | Error e, _ | _, Error e -> Error e)
  | Let (name, value, body) ->
     (match infer_type value typing with
      | Ok typ -> infer_type body (TypeMap.add name typ typing)
      | Error e -> Error e)
  | Lam (name, typ, body) ->
     (match infer_type body (TypeMap.add name typ typing) with
      | Ok body_typ -> Ok (Function (typ, body_typ))
      | Error e -> Error e)
  | App (func, arg) ->
     let func_type = infer_type func typing in
     let arg_type = infer_type arg typing in
     (match func_type, arg_type with
      | Ok (Function (should_arg_type, out_type)), Ok arg_type ->
         if should_arg_type = arg_type then
           Ok out_type
         else
           Error (TypeError "In function application, input type of function and type of argument mismatch")
      | Ok Integer, _ -> Error (TypeError "In function application, lhs is not of type fun")
      | Error e, _ | _, Error e -> Error e)

let typecheck expr = infer_type expr TypeMap.empty
