module Seq = struct
  include Seq

  (* Sequence of the n first numbers. *)
  let ordinal n =
    let rec aux i () =
      if i >= n then Nil
      else Cons (i, aux (i+1))
    in
    aux 0
end

(** Generators. *)
module Generator = struct
  type name = string

  (** A generator. *)
  type t =
    {
      opts : (string * string) list; (** list of optional parameters and their value (a=b) *)
      name : name;
      shape : [`Circle | `Wire]; (** shape of the node *)
      source : int array; (** horizontal position of the source ports *)
      target : int array; (** horizontal position of the target ports *)
      mutable y : int; (** vertical position *)
    }

  let name g = g.name

  let create ?(options=[]) name source target =
    let shape = ref `Circle in
    List.iter
      (function
        | "shape", "wire" -> assert (source = 1 && target = 1); shape := `Wire
        | l, v -> Printf.printf "Unknown option %s%s%s!\n%!" l (if v = "" then "" else "=") v
      ) options;
    (* TODO: parse options *)
    {
      opts = options;
      shape = !shape;
      name;
      source = Array.init source (fun _ -> 0);
      target = Array.init target (fun _ -> 0);
      y = 0;
    }

  let source g = Array.length g.source

  let target g = Array.length g.target

  let id () = create ~options:["shape","wire"] "1" 1 1
end

(** Expression for a cell. *)
type expr =
  | Gen of Generator.name (** a generator *)
  | Obj of int (** an object *)
  | Comp of int option * expr * expr (** composite in maximal codimension - 1 *)

(** Declarations. *)
type t =
  {
    gens : (Generator.name * Generator.t) list; (** gneerators *)
    cells : (int * expr) list (** cells (which are numbered in order to be able to identify them in LaTeX) *)
  }

(** Empty declaration list. *)
let decls_empty = { gens = []; cells = [] }

(** Add a generator. *)
let add_gen l g =
  { l with gens = (Generator.name g, g)::l.gens }

(** Dimension of a cell. *)
let rec dim = function
  | Gen _ -> 2
  | Obj _ -> 1
  | Comp (_, e1, e2) -> max (dim e1) (dim e2)

(** Typing error. *)
exception Typing of string

(** Type of a cell. *)
let rec typ gens = function
  | Gen s ->
    (
      try
        let g = List.assoc s gens in
        Generator.source g, Generator.target g
      with
      | Not_found -> raise (Typing ("unknown cell " ^ s))
    )
  | Obj n -> n, n
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

(* let rec generators = function *)
  (* | Gen g -> [g] *)
  (* | Obj _ -> [] *)
  (* | Comp (_, f, g) -> (generators f)@(generators g) *)

(** Add a cell to declarations. *)
let add_cell l (id,cell) =
  ignore (typ l.gens cell);
  { l with cells = (id,cell)::l.cells }

(** Normalized form for expressions. *)
module Stack = struct
  type slice = Generator.t list

  type t = slice list

  let rec create env e =
    let id n = List.init n (fun _ -> Generator.id ()) in
    match e with
    | Gen g -> [[List.assoc g env]]
    | Obj n -> [id n]
    | Comp (_, f, Obj n) -> List.map (fun f -> f@(id n)) (create env f)
    | Comp (_, Obj n, f) -> List.map (fun f -> (id n)@f) (create env f)
    | Comp (Some 0,f,g) ->
      let f = create env f in
      let g = create env g in
      assert (List.length f = 1);
      assert (List.length g = 1);
      [(List.hd f)@(List.hd g)]
    | Comp (_,f,g) -> (create env f)@(create env g)

  let create env e =
    let ans = create env e in
    (* Set the vertical position *)
    List.iteri
      (fun i f ->
         List.iter (fun g -> g.Generator.y <- i) f
      ) ans;
    ans
end

(*
(** Compute the placement of cells. *)
module Typeset = struct
  type t =
    {
      env : (string * Generator.t) list; (** declared generators *)
      expr : expr; (** the cell *)
      pos : (Port.t * (int * int) ref) list; (** position of a port *)
      source : string; (** fake generator for the source *)
      target : string; (** fake generator for the target *)
    }

  let create env expr =
    let s, t = typ env expr in
    let source = "@src" in
    let target = "@tgt" in
    let env = (source, Generator.create 0 s)::(target, Generator.create t 0)::env in
    let expr' = Comp (Some 1, Gen source, Comp (Some 1, expr, Gen target)) in
    let pos = Port.all env expr' in
    let pos = List.flatten pos in
    let pos = List.map (fun p -> p, ref (0,0)) pos in
    { env; expr; pos; source; target }

  (** All the generators. *)
  let generators t =
    generators t.expr

  (** Position of a port. *)
  let position t p =
    snd (List.find (fun (p',_) -> Port.eq p p') t.pos)

  (** Generate constraints on ports. *)
  let constraints t =
    let ans = ref [] in
    let add c = ans := c :: !ans in
    List.iter
      (fun g ->
         let gen = List.assoc g t.env in
         for i = 0 to Generator.source gen - 1 do
           for j = 0 to Generator.target gen - 1 do
             add (`Next (g,`Source,i) (g,`Source,j))
           done
         done
      ) (generators t)
end
*)
