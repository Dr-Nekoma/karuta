%{
%}

%token <int32> LITERAL_INT
%token <string> LITERAL_STRING
%token <string> IDENT
%token <string> UPPER_IDENT
%token LEFT_DELIM
%token RIGHT_DELIM
%token COMMA
%token EOF

%start <Ast.t option> program
%%

program:
  | EOF
    { None }
  | s = statement; EOF
    { Some s }
  ;

statement:
  | functor_name = IDENT; LEFT_DELIM; identifiers = list_identifiers; RIGHT_DELIM { Ast.Functor { name = functor_name; elements = identifiers; arity = List.length identifiers } }
  ;

list_identifiers:
  | IDENT { [Ast.Functor {name = $1; elements = []; arity = 0}] }
  | UPPER_IDENT { [Ast.Variable {name = $1}] }
  | list_identifiers COMMA IDENT { $1 @ [Ast.Functor {name = $3; elements = []; arity = 0}] }
  | list_identifiers COMMA UPPER_IDENT { $1 @ [Ast.Variable {name = $3}] }

value:
  | i = LITERAL_INT
    { Ast.VInteger i}
  | s = LITERAL_STRING
    { Ast.VString s}
  ;
