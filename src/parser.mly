%{
    open Lang
%}

%token LACC RACC LPAR RPAR LBRA RBRA EQ COLON COMMA EOF
%token GEN TO
%token <int> CELL
%token <int> INT
%token <int option> COMP
%token <string> STRING

%start decls
%type <Lang.t> decls
%right COMP
%%

decls:
  | indecls EOF { $1 }

indecls:
  | indecls gen { add_gen $1 $2 }
  | indecls cell { add_cell $1 $2 }
  | { decls_empty () }

gen:
  | GEN opts LACC STRING COLON INT TO INT RACC { Generator.create $4 $6 $8 $2 }

opts:
  | { [] }
  | LBRA inopts RBRA { $2 }

inopts:
  | opt { [$1] }
  | opt COMMA inopts { $1::$3 }

opt:
  | STRING { $1,"" }
  | STRING EQ STRING { $1, $3 }

cell:
  | CELL LACC expr RACC { $1,$3 }

expr:
  | base { $1 }
  | expr COMP expr { Comp (1,$1,$3) }
  | LPAR hexpr RPAR { $2 }

hexpr:
  | base { $1 }
  | hexpr COMP hexpr { Comp (0,$1,$3) }

base:
  | STRING { GName $1 }
  | INT { Id $1 }
  | LPAR INT TO INT RPAR opts { Gen (G.create (Printf.sprintf "(%d->%d)" $2 $4) $2 $4 $6) }
