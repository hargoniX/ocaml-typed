%{
 %}

%token LPAREN
%token RPAREN
%token EQ
%token PLUS
%token IN
%token LAMBDA
%token ARROW
%token EOF
%token LET
%token <string> ID
%token <int> INT

%start <Expression.expression> program
%%
program:
  | s = expr; EOF { s }

expr:
  | LPAREN; e = expr; RPAREN { e }
  | e = INT { IntLit e }
  | el = expr; PLUS; er = expr { Add (el, er) }
  | LET; var = ID; EQ; value = expr; IN; body = expr { Let (var, value, body) }
  | LAMBDA; var = ID; ARROW; body = expr { Lam (var, body) }
  | f = expr; arg = expr { App (f, arg) }
  | ident = ID { Var ident }
