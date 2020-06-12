(** Generators. *)
module Generator = struct
  (** A generator. *)
  type t =
    {
      opts : (string * string) list; (** list of optional parameters and their value (a=b) *)
      typ : int * int; (** type: number of inputs and outputs *)
    }

  let create ?(options=[]) source target =
    { opts = options; typ = (source, target) }

  let source g = fst g.typ

  let target g = snd g.typ
end

(** Expression for a cell. *)
type expr =
  | Gen of string (** a generator *)
  | Id of int (** identity *)
  | Comp of int option * expr * expr (** composite, the number is the dimension in which they are composed (max-1 if not specified) *)

(** Declarations. *)
type t =
  {
    gens : (string * Generator.t) list; (** gneerators *)
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
        (List.assoc s gens).Generator.typ
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

let rec generators = function
  | Gen g -> [g]
  | Id _ -> []
  | Comp (_, f, g) -> (generators f)@(generators g)

(** Add a cell to declarations. *)
let add_cell l (id,cell) =
  ignore (typ l.gens cell);
  { l with cells = (id,cell)::l.cells }

(** Compute the placement of cells. *)
module Typeset = struct
  module Port = struct
    (** A port of a generator. *)
    type t = string * [`Source | `Target] * int

    (** Equality on ports. *)
    let eq (g, d, n) (g', d', n') =
      (* Note that we compare names using physical equality! *)
      g == g' && d = d' && n = n'

    (** All ports of an expression. *)
    let all env e =
      List.map
        (fun g ->
           let gen = List.assoc g env in
           let s = Generator.source gen in
           let t = Generator.target gen in
           let s = List.init s (fun i -> g, `Source, i) in
           let t = List.init t (fun i -> g, `Target, i) in
           s@t
        ) (generators e)

  end

  type t =
    {
      env : (string * Generator.t) list; (** declared generators *)
      expr : expr; (** the cell *)
      ports : (Port.t * int ref) list; (** position of the n-th source or target *)
      source : string; (** fake generator for the source *)
      target : string; (** fake generator for the target *)
    }

  let create env expr =
    let s, t = typ env expr in
    let source = "@src" in
    let target = "@tgt" in
    let env = (source, Generator.create 0 s)::(target, Generator.create t 0)::env in
    let expr' = Comp (Some 1, Gen source, Comp (Some 1, expr, Gen target)) in
    let ports = Port.all env expr' in
    let ports = List.flatten ports in
    let ports = List.map (fun p -> p, ref 0) ports in
    { env; expr; ports; source; target }
end
