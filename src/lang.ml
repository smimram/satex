(** Definition of a generator. *)
type gen =
  {
    gen_opts : (string * string) list; (** list of optional parameters and their value (a=b) *)
    gen_typ : int * int; (** type: number of inputs and outputs *)
  }

(** Expression for a cell. *)
type expr =
  | Gen of string (** a generator *)
  | Id of int (** identity *)
  | Comp of int option * expr * expr (** composite, the number is the dimension in which they are composed (max-1 if not specified) *)

(** Declarations. *)
type t =
  {
    gens : (string * gen) list; (** gneerators *)
    cells : (int * expr) list (** cells (which are numbered in order to be able to identify them in LaTeX) *)
  }

(** Empty declaration list. *)
let decls_empty = { gens = []; cells = [] }

(** Add a generator. *)
let add_gen l (name,gen) =
  { l with gens = (name,gen)::l.gens }

(** Dimension of a cell. *)
let rec dim = function
  | Gen _ -> 2
  | Id _ -> 1
  | Comp (_, e1, e2) -> max (dim e1) (dim e2)

(** Typing error. *)
exception Typing of string

(** Type of a cell. *)
let rec typ gens = function
  | Gen s ->
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
    match n with
    | 0 -> s1+s2, t1+t2
    | 1 ->
        (
          if t1 <> s2 then
            let err = Printf.sprintf "cannot compose target %d with source %d" t1 s2 in
            raise (Typing err)
        );
        s1, t2
    | _ ->
      (* n-dimensional diagrams with n>2 are not (yet) supported *)
      assert false

(** Add a cell to declarations. *)
let add_cell l c =
  ignore (typ l.gens (snd c));
  { l with cells = c::l.cells }

(* module Matrix = struct *)
  (* type t =  *)
(* end *)
