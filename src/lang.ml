(** Definition of a generator. *)
type gen =
  {
    gen_opts : (string * string option) list;
    gen_typ : int * int;
  }

(** Expression for a cell. *)
type expr =
  | Ident of string
  | Id of int
  | Comp of int option * expr * expr

(** Declarations. *)
type t =
  {
    gens : (string * gen) list;
    cells : (int * expr) list
  }

let decls_empty = { gens = []; cells = [] }

let add_gen l d =
  { l with gens = d::l.gens }

let rec dim = function
  | Ident _ -> 2
  | Id _ -> 1
  | Comp (_, e1, e2) -> max (dim e1) (dim e2)

exception Typing of string

let rec typ gens = function
  | Ident s ->
     (
       try
         (List.assoc s gens).gen_typ
       with
       | Not_found -> raise (Typing ("unknown cell " ^ s))
     )
  | Id n -> n, n
  | Comp (n, e1, e2) ->
     let n =
       match n with
       | Some n -> n
       | None -> min (dim e1) (dim e2) - 1
     in
     let s1, t1 = typ gens e1 in
     let s2, t2 = typ gens e2 in
     if n = 0 then (s1+s2, t1+t2)
     else if n = 1 then
       (
         (
           if t1 <> s2 then
            let err = Printf.sprintf "cannot compose target %d with source %d" t1 s2 in
            raise (Typing err)
         );
         s1, t2
       )
     else assert false

let add_cell l c =
  ignore (typ l.gens (snd c));
  { l with cells = c::l.cells }
