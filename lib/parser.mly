%{
%}

%token <int32> LITERAL_INT
%token <string> LITERAL_STRING
%token <string> IDENT
%token <string> UPPER_IDENT
%token LEFT_DELIM
%token RIGHT_DELIM
%token COMMA
%token DOT
%token HOLDS
%token EOF

%start <Ast.t list> program
%%

program:
  | declaration program
    {
      ($1 :: $2) }
  | EOF
    { [] }
  ;

functorr:
  | functor_name = IDENT; LEFT_DELIM; identifiers = list_identifiers; RIGHT_DELIM
  { ({ namef = functor_name; elements = identifiers; arity = List.length identifiers } : Ast.func) }
  ;

declaration:
  | functor_elem = functorr; DOT
    {
      print_endline "Something!";
      print_endline @@ Ast.show_func functor_elem;
      print_endline "Another Something!";      
      Ast.Declaration {head = functor_elem; body = []}}
  | functor_elem = functorr; HOLDS; statements = separated_nonempty_list(COMMA, functorr); DOT
    { Ast.Declaration { head = functor_elem; body = statements } }
  ;

list_identifiers:
  | IDENT { [Ast.Functor {namef = $1; elements = []; arity = 0}] }
  | UPPER_IDENT { [Ast.Variable {namev = $1}] }
  | list_identifiers COMMA IDENT { $1 @ [Ast.Functor {namef = $3; elements = []; arity = 0}] }
  | list_identifiers COMMA UPPER_IDENT { $1 @ [Ast.Variable {namev = $3}] }

value:
  | i = LITERAL_INT
    { Ast.VInteger i}
  | s = LITERAL_STRING
    { Ast.VString s}
