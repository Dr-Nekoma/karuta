%{
%}

%token <string> IDENT
%token <string> UPPER_IDENT
%token LEFT_DELIM
%token RIGHT_DELIM
%token COMMA
%token DOT
%token HOLDS
%token EOF
%token QUERY

%start <Ast.ParserClause.t list> program
%%

program:
  | declaration program
    { ($1 :: $2) }
  | query program
    { ($1 :: $2) }
  | EOF
    { [] }
  ;

functorr:
  | functor_name = IDENT; LEFT_DELIM; identifiers = list_identifiers
  { ({ namef = functor_name; elements = identifiers; arity = List.length identifiers } : Ast.func) }
  ;

declaration:
  | functor_elem = functorr; DOT
    { Ast.Declaration {head = functor_elem; body = []}}
  | functor_elem = functorr; HOLDS; statements = separated_nonempty_list(COMMA, functorr); DOT
    { Ast.Declaration { head = functor_elem; body = statements } }
  ;

query:
  | functor_elem = functorr; QUERY
    { Ast.Query functor_elem}
  ;

list_identifiers:
  | RIGHT_DELIM { [] }
  | UPPER_IDENT COMMA list_identifiers { Ast.Variable {namev = $1} :: $3 }
  | IDENT COMMA list_identifiers { Ast.Functor {namef = $1; elements = []; arity = 0} :: $3 }
  | functor_elem = functorr; COMMA list_identifiers { (Ast.Functor functor_elem) :: $3 }
  | IDENT RIGHT_DELIM { [Ast.Functor {namef = $1; elements = []; arity = 0}] }
  | functor_elem = functorr; RIGHT_DELIM { [Ast.Functor functor_elem] }
  | UPPER_IDENT RIGHT_DELIM { [Ast.Variable {namev = $1}] }
