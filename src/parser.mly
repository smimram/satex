%{
    open Lang

    module Generator = struct
      include Generator

      let env = ref []

      let add n s t o =
        env := (n, create n s t o) :: !env

      let find n =
        List.assoc n !env
    end
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
  | gen indecls { $2 }
  | cell indecls { Printf.printf "add cell %d: %s\n%!" (fst $1) (string_of_expr (snd $1)); $1::$2 }
  | { [] }

gen:
  | GEN opts LACC STRING COLON INT TO INT RACC { Printf.printf "add generator %s\n%!" $4; Generator.add $4 $6 $8 $2 }

opts:
  | { [] }
  | LBRA inopts RBRA { List.rev $2 }

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
  | STRING { Gen (Generator.find $1) }
  | INT { Id $1 }
  | LPAR INT TO INT RPAR opts { Gen (Generator.create (Printf.sprintf "(%d->%d)" $2 $4) $2 $4 $6) }
