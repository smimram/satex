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
%left COMP
%%

decls:
  | indecls EOF { $1 }

indecls:
  | indecls gen { add_gen $1 $2 }
  | indecls cell { add_cell $1 $2 }
  | { decls_empty }

gen:
  | GEN opts LACC STRING COLON INT TO INT RACC { Generator.create ~options:$2 $4 $6 $8 }

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
  | STRING { Gen $1 }
  | INT { Obj $1 }
  | expr COMP expr { Comp ($2,$1,$3) }
  | LPAR expr RPAR { $2 }
