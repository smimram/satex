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
%%

decls:
    | indecls EOF { $1 }

indecls:
    | indecls gen { add_gen $1 $2 }
    | indecls cell { add_cell $1 $2 }
    | { decls_empty }

gen:
    | GEN opts LACC STRING COLON INT TO INT RACC { $4, { gen_opts = $2; gen_typ = ($6, $8) } }

opts:
    | { [] }
    | LBRA inopts RBRA { $2 }

inopts:
    | opt { [$1] }
    | opt COMMA inopts { $1::$3 }

opt:
    | STRING { $1,None }

cell:
    | CELL LACC expr RACC { $1,$3 }

expr:
    | STRING { Ident $1 }
    | INT { Id $1 }
    | expr COMP expr { Comp ($2,$1,$3) }
    | LPAR expr RPAR { $2 }
