open Extlib

(** Generators. *)
module Generator = struct
  type name = string

  (** A generator. *)
  type t =
    {
      options : (string * string) list; (** list of optional parameters and their value (a=b) *)
      name : name;
      shape : [`Circle | `Wire | `Cap]; (** shape of the node *)
      source : float array; (** horizontal position of the source ports *)
      target : float array; (** horizontal position of the target ports *)
      mutable y : float; (** vertical position *)
    }

  let name g = g.name

  let copy g =
    {
      options = g.options;
      name = g.name;
      shape = g.shape;
      source = Array.copy g.source;
      target = Array.copy g.target;
      y = g.y;
    }

  let create ?(options=[]) name source target =
    let shape = ref `Circle in
    (* Normalize options *)
    let optn =
      [
        ("cap",""),("shape","cap");
        ("cup",""),("shape","cup")
      ]
    in
    let options = List.map (fun o -> try List.assoc o optn with Not_found -> o) options in
    List.iter
      (function
        | "shape", "wire" -> assert (source = 1 && target = 1); shape := `Wire
        | "shape", "cap"
        | "shape", "cup" -> assert ((source = 2 && target = 0) || (source = 0 && target = 2)); shape := `Cap
        | l, v -> Printf.printf "Unknown option %s%s%s!\n%!" l (if v = "" then "" else "=") v
      ) options;
    (* TODO: parse options *)
    {
      options;
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

let satix_fname = ref "out.satix"

(** Declarations. *)
type t =
  {
    fname : string; (** file to output *)
    gens : (G.name * G.t) list; (** generators *)
    cells : (int * expr) list (** cells (which are numbered in order to be able to identify them in LaTeX) *)
  }

(** Empty declaration list. *)
let decls_empty () = { fname = !satix_fname; gens = []; cells = [] }

(** Add a generator. *)
let add_gen l g =
  { l with gens = l.gens@[G.name g, g] }

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

(** Add a cell to declarations. *)
let add_cell l (id,cell) =
  ignore (typ l.gens cell);
  { l with cells = l.cells@[id,cell] }

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
        (* Match targets with next sources. *)
        assert (len = ArrayList.length s);
        for i = 0 to len - 1 do
          let j = max (ArrayList.get s i) (ArrayList.get t i) in
          ArrayList.set s i j;
          ArrayList.set t i j
        done;
        stack g
    in
    let f = ref f in
    for i = 0 to 1 do
      stack !f
    done

  module type Draw = sig
    (** A device for drawing. *)
    type t

    (** Start outputing in given filename morphism with given id. *)
    val create : string -> int -> t
    val close : t -> unit
    val line : t -> float * float -> float * float -> unit
    val arc : t -> float * float -> float * float -> float * float -> unit
  end

  module DrawGraphics = struct
    let scale x = int_of_float (x*.100.)
    let xscale x = scale x
    let yscale y = Graphics.size_y () - scale y

    type t = unit

    let create fname id =
      Graphics.open_graph ""

    let close () =
      ignore (Graphics.wait_next_event [Graphics.Key_pressed])

    let line () (x1,y1) (x2,y2) =
      Graphics.moveto (xscale x1) (yscale y1);
      Graphics.lineto (xscale x2) (yscale y2)

    let arc () (x,y) (rx,ry) (a,b) =
      let a = int_of_float a in
      let b = int_of_float b in
      Graphics.draw_arc (xscale x) (yscale y) (scale rx) (scale ry) a b
  end

  module DrawTikz = struct
    type t = out_channel

    let create fname id =
      let oc = open_out_gen [Open_creat; Open_append] 0o644 fname in
      output_string oc (Printf.sprintf "\\defsatexfig{%d}{\\begin{tikzpicture}[yscale=-1] " id);
      oc

    let close oc =
      output_string oc "\\end{tikzpicture}}\n";
      close_out oc

    let line oc (x1,y1) (x2,y2) =
      output_string oc (Printf.sprintf "\\draw (%f,%f) -- (%f,%f); " x1 y1 x2 y2)

    let arc oc (x,y) (rx,ry) (a,b) =
      let a = -.a in
      let b = -.b in
      (* The starting point is supposed to be horizontal for now... *)
      assert (int_of_float a mod 180 = 0);
      output_string oc (Printf.sprintf "\\draw ([shift=(0:%f)]%f,%f) arc (%f:%f:%f and %f); " rx x y a b rx ry)
  end

  let draw device id f =
    let m, fname =
        match device with
          | `Graphics -> (module DrawGraphics : Draw), ""
          | `Tikz fname -> (module DrawTikz : Draw), fname
    in
    let module Draw = (val m : Draw) in
    let d = Draw.create fname id in
    let draw_generator g =
      let y = g.G.y in
      if g.G.shape = `Wire then
        Draw.line d (g.G.source.(0),y) (g.G.target.(0),g.G.y+.1.)
      else if g.G.shape = `Cap then
        if Array.length g.G.source = 2 then
          let l = g.G.source.(1) -. g.G.source.(0) in
          let x = Float.mean g.G.source.(0) g.G.source.(1) in
          Draw.arc d (x,y) (l /. 2., 0.5) (0.,-180.)
        else
          let l = g.G.target.(1) -. g.G.target.(0) in
          let x = Float.mean g.G.target.(0) g.G.target.(1) in
          Draw.arc d (x,y+.1.) (l /. 2., 0.5) (0.,180.)
      else
        let c =
          if G.source g > 0 then Float.mean g.G.source.(0) (Array.last g.G.source)
          else (g.G.target.(0) +. Array.last g.G.target) /. 2.
        in
        Array.iter (fun x -> Draw.line d (x,y) (c,y+.0.5)) g.G.source;
        Array.iter (fun x -> Draw.line d (c,y+.0.5) (x,y+.1.)) g.G.target;
    in
    List.iter (List.iter draw_generator) f;
    Draw.close d
end

let draw l =
  (try Sys.remove l.fname with _ -> ());
  List.iter
    (fun (id,e) ->
       let f = Stack.create l.gens e in
       Stack.typeset f;
       Stack.draw (`Tikz l.fname) id f
    ) l.cells
