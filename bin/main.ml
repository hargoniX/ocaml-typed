open Typed;;
open Eval;;
open Typecheck;;

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.lex lexbuf with
  | Lexer.Error msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let read_input () =
  try read_line () with
  | End_of_file ->
     print_string "Byeeee o/\n";
     exit 0

let rep () =
  print_string "> ";
  let input = read_input () in
  if input = "" then
    ()
  else
    let lexbuf = Lexing.from_string input in
    let expr = (parse_with_error lexbuf) in
    (match typecheck expr with
     | Ok _ ->
       let base_env = { next = None; variable = { name = "_"; value = Eval.Const 0 }} in
       let evaled = eval expr base_env in
       (match evaled with
        | Error _ -> print_string "The typecherk said it was fine!!!"
        | Ok (Const v) -> print_int v
        | Ok (Fun _) -> print_string "Got function")
     | Error e -> print_error e);
    print_newline ()

let rec repl () =
  rep ();
  repl ()

let () = repl ()
