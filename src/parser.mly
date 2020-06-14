%{
    open Extlib
    open Lang

    module Generator = struct
      include Generator

      let env = ref []

      let add n s t o =
        env := (n, create n s t o) :: !env

      let find n =
        try List.assoc n !env
        with Not_found -> failwith ("Could not find generator "^n)

      let anonymous s t o =
        create (Printf.sprintf "(%d->%d)" s t) s t o

      let label o =
        let n =
          List.count
            (function
             | "label", _ -> true
             | l, "" when String.length l >= 2 && l.[0] = '"' && String.last l = '"' -> true
             | _ -> false
            ) o
        in
        let o = ["shape", "label"] in
        anonymous n n o
    end
%}

%token LACC RACC LPAR RPAR LBRA RBRA EQ COLON COMMA COMP EOF
%token GEN TO LABEL
%token <int> CELL
%token <int> INT
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
  | STRING opts { Gen (Generator.add_options (Generator.find $1) $2) }
  | LABEL opts { Gen (Generator.label $2) }
  | INT { Id $1 }
  | LPAR INT TO INT RPAR opts { Gen (Generator.anonymous $2 $4 $6) }
