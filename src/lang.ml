module Array = struct
  include Array

  let last a =
    a.(Array.length a - 1)
end

module List = struct
  include List

  let rec last = function
    | [] -> raise Not_found
    | [x] -> x
    | _::l -> last l
end

module Float = struct
  include Float

  let mean x y = (x +. y) /. 2.
end

(** Operations on lists of arrays. *)
module ArrayList = struct
  type 'a t = 'a array list

  let length l =
    List.fold_left (fun n a -> n + Array.length a) 0 l

  let rec get l n =
    match l with
    | a::l ->
      let len = Array.length a in 
      if n < len then a.(n)
      else get l (n-len)
    | [] -> raise Not_found

  let rec set l n v =
    match l with
    | a::l ->
      let len = Array.length a in
      if n < len then a.(n) <- v
      else set l (n-len) v
    | [] -> raise Not_found
end

(** Generators. *)
module Generator = struct
  type name = string

  (** A generator. *)
  type t =
    {
      opts : (string * string) list; (** list of optional parameters and their value (a=b) *)
      name : name;
      shape : [`Circle | `Wire | `Cap]; (** shape of the node *)
      source : float array; (** horizontal position of the source ports *)
      target : float array; (** horizontal position of the target ports *)
      mutable y : float; (** vertical position *)
    }

  let name g = g.name

  let copy g =
    {
      opts = g.opts;
      name = g.name;
      shape = g.shape;
      source = Array.copy g.source;
      target = Array.copy g.target;
      y = g.y;
    }

  let create ?(options=[]) name source target =
    let shape = ref `Circle in
    List.iter
      (function
        | "shape", "wire" -> assert (source = 1 && target = 1); shape := `Wire
        | "shape", "cap"
        | "shape", "cup" -> assert ((source = 2 && target = 0) || (source = 0 && target = 2)); shape := `Cap
        | l, v -> Printf.printf "Unknown option %s%s%s!\n%!" l (if v = "" then "" else "=") v
      ) options;
    (* TODO: parse options *)
    {
      opts = options;
      shape = !shape;
      name;
      source = Array.init source (fun _ -> 0.);
      target = Array.init target (fun _ -> 0.);
      y = 0.;
    }

  let source g = Array.length g.source

  let target g = Array.length g.target

  let id () = create ~options:["shape","wire"] "1" 1 1
end

module G = Generator

(** Expression for a cell. *)
type expr =
  | Gen of G.name (** a generator *)
  | Obj of int (** an object *)
  | Comp of int option * expr * expr (** composite in maximal codimension - 1 *)

(** Declarations. *)
type t =
  {
    gens : (G.name * G.t) list; (** generators *)
    cells : (int * expr) list (** cells (which are numbered in order to be able to identify them in LaTeX) *)
  }

(** Empty declaration list. *)
let decls_empty = { gens = []; cells = [] }

(** Add a generator. *)
let add_gen l g =
  { l with gens = (G.name g, g)::l.gens }

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
        G.source g, G.target g
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
  type slice = G.t list

  type t = slice list

  let to_string f =
    let generator g =
      let s = Array.to_list g.G.source in
      let t = Array.to_list g.G.target in
      let s = String.concat "," (List.map string_of_float s) in
      let t = String.concat "," (List.map string_of_float t) in
      Printf.sprintf "(%s)->(%s)" s t
    in
    let slice f = String.concat ", " (List.map generator f) in
    String.concat "\n" (List.map slice f)

  let rec create env e =
    let id n = List.init n (fun _ -> G.id ()) in
    match e with
    | Gen g -> [[Generator.copy (List.assoc g env)]]
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

  let create env e : t =
    let ans = create env e in
    (* Set the vertical position *)
    List.iteri
      (fun i f ->
         List.iter (fun g -> g.G.y <- float_of_int i) f
      ) ans;
    ans

  let sources f =
    List.map (fun g -> g.G.source) (List.hd f)

  let targets f =
    List.map (fun g -> g.G.target) (List.last f)

  let typeset f =
    let last_source = ref (-1.) in
    let last_target = ref (-1.) in
    let generator g =
      (* Space the sources *)
      if Array.length g.G.source > 0 then
        (
          g.G.source.(0) <- max (!last_source +. 1.) g.G.source.(0);
          last_source := g.G.source.(0)
        );
      for i = 0 to Array.length g.G.source - 2 do
        g.G.source.(i+1) <- max (g.G.source.(i) +. 1.) g.G.source.(i+1);
        last_source := g.G.source.(i+1)
      done;
      (* Propagate in morphism *)
      (
        match g.G.shape with
        | `Wire ->
          g.G.target.(0) <- max g.G.source.(0) g.G.target.(0);
          g.G.source.(0) <- g.G.target.(0)
        | _ -> ()
      );
      (* Space up the targets *)
      if Array.length g.G.target > 0 then
        (
          g.G.target.(0) <- max (!last_target +. 1.) g.G.target.(0);
          last_target := g.G.target.(0)
        );
      for i = 0 to Array.length g.G.target - 2 do
        g.G.target.(i+1) <- max (g.G.target.(i) +. 1.) g.G.target.(i+1);
        last_target := g.G.target.(i+1)
      done
    in
    let slice f =
      last_source := (-1.);
      last_target := (-1.);
      List.iter generator f
    in
    let rec stack = function
      | [] -> assert false
      | [f] -> slice f
      | f::g ->
        slice f;
        let t = targets [f] in
        let s = sources g in
        let len = ArrayList.length t in
        assert (len = ArrayList.length s);
        for i = 0 to len - 1 do
          ArrayList.set s i (ArrayList.get t i)
        done;
        stack g
    in
    let f = ref f in
    for i = 0 to 1 do
      stack !f
    done

  module Draw = struct
    let scale x = int_of_float (x*.100.)
    let xscale x = scale x
    let yscale y = Graphics.size_y () - scale y

    let line (x1,y1) (x2,y2) =
      Graphics.moveto (xscale x1) (yscale y1);
      Graphics.lineto (xscale x2) (yscale y2)

    let arc (x,y) (rx,ry) (a,b) =
      Graphics.draw_arc (xscale x) (yscale y) (scale rx) (scale ry) a b
  end

  let draw f =
    Graphics.open_graph "";
    let draw_generator g =
      let y = g.G.y in
      if g.G.shape = `Wire then
        Draw.line (g.G.source.(0),y) (g.G.target.(0),g.G.y+.1.)
      else if g.G.shape = `Cap then
        if Array.length g.G.source = 2 then
          let d = g.G.source.(1) -. g.G.source.(0) in
          let x = Float.mean g.G.source.(0) g.G.source.(1) in
          Draw.arc (x,y) (d /. 2., 0.5) (0,-180)
        else
          let d = g.G.target.(1) -. g.G.target.(0) in
          let x = Float.mean g.G.target.(0) g.G.target.(1) in
          Draw.arc (x,y+.1.) (d /. 2., 0.5) (0,180)
      else
        let c =
          if G.source g > 0 then Float.mean g.G.source.(0) (Array.last g.G.source)
          else (g.G.target.(0) +. Array.last g.G.target) /. 2.
        in
        Array.iter (fun x -> Draw.line (x,y) (c,y+.0.5)) g.G.source;
        Array.iter (fun x -> Draw.line (c,y+.0.5) (x,y+.1.)) g.G.target;
    in
    List.iter (List.iter draw_generator) f
end

(*
(** Compute the placement of cells. *)
module Typeset = struct
  type t =
    {
      env : (string * G.t) list; (** declared generators *)
      expr : expr; (** the cell *)
      pos : (Port.t * (int * int) ref) list; (** position of a port *)
      source : string; (** fake generator for the source *)
      target : string; (** fake generator for the target *)
    }

  let create env expr =
    let s, t = typ env expr in
    let source = "@src" in
    let target = "@tgt" in
    let env = (source, G.create 0 s)::(target, G.create t 0)::env in
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
         for i = 0 to G.source gen - 1 do
           for j = 0 to G.target gen - 1 do
             add (`Next (g,`Source,i) (g,`Source,j))
           done
         done
      ) (generators t)
end
*)
