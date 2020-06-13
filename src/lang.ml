open Extlib

(** Generators. *)
module Generator = struct
  type name = string

  (** A generator. *)
  type t =
    {
      options : (string * string) list; (** list of optional parameters and their value (a=b) *)
      name : name;
      label : string;
      shape : [`Circle | `None | `Cap]; (** shape of the node *)
      source : float array; (** horizontal position of the source ports *)
      target : float array; (** horizontal position of the target ports *)
      mutable y : float; (** vertical position *)
    }

  let name g = g.name

  let copy g =
    {
      options = g.options;
      name = g.name;
      label = g.label;
      shape = g.shape;
      source = Array.copy g.source;
      target = Array.copy g.target;
      y = g.y;
    }

  let create name source target options =
    let shape = ref `Circle in
    let label = ref "" in
    (* Set default options. *)
    let options = ["labelwidth", ".5"; "labelheight", ".5"; "arrow", "none"]@options in
    let options =
      (* 1->1 are rigid by default *)
      if source = 1 && target = 1 then ["rigid","true"]@options
      else options
    in
    (* Normalize options. *)
    let options =
      List.map
        (function
          | "arrow","" -> "arrow","right"
          | lv -> lv
        ) options
    in
    (* Parse options. *)
    List.iter
      (function
        | "shape", "none" ->
          assert (source = 1 && target = 1); shape := `None
        | "cap", ""
        | "cup", ""
        | "shape", "cap"
        | "shape", "cup" ->
          assert ((source = 2 && target = 0) || (source = 0 && target = 2));
          shape := `Cap
        | l, _ when String.length l >= 2 && l.[0] = '"' && l.[String.length l - 1] = '"' ->
          label := String.sub l 1 (String.length l - 2)
        | l, v ->
          (* Printf.printf "Unknown option %s%s%s!\n%!" l (if v = "" then "" else "=") v *)
          ()
      ) options;
    let options = List.rev options in
    {
      options;
      name;
      label = !label;
      shape = !shape;
      source = Array.init source (fun _ -> 0.);
      target = Array.init target (fun _ -> 0.);
      y = 0.;
    }

  let source g = Array.length g.source

  let target g = Array.length g.target

  let id () = create "1" 1 1 ["shape","none";"rigid","true"]

  let rigid g = List.mem_assoc "rigid" g.options

  let get g o = List.assoc o g.options

  let get_float g o = float_of_string (get g o)
end

module G = Generator

(** Expression for a cell. *)
type expr =
  | GName of G.name (** a generator name *)
  | Gen of G.t (** a generator *)
  | Id of int (** an object *)
  | Comp of int * expr * expr (** composite in given dimension *)

let rec string_of_expr ?(p=false) = function
  | GName g -> g
  | Gen g -> Printf.sprintf "(%d->%d)" (G.source g) (G.target g)
  | Id n -> string_of_int n
  | Comp (n, f, g) ->
    Printf.sprintf "%s%s *%d %s%s"
      (if p then "(" else "")
      (string_of_expr ~p:true f)
      n
      (string_of_expr ~p:true g)
      (if p then ")" else "")

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

(** Typing error. *)
exception Typing of string

(** Type of a cell. *)
let rec typ gens = function
  | GName s ->
    (
      try
        let g = List.assoc s gens in
        G.source g, G.target g
      with
      | Not_found -> raise (Typing ("unknown cell " ^ s))
    )
  | Gen g -> G.source g, G.target g
  | Id n -> n, n
  | Comp (n, e1, e2) ->
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
  Printf.printf "add cell %d: %s\n%!" id (string_of_expr cell);
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
    | GName g -> [[Generator.copy (List.assoc g env)]]
    | Gen g -> [[Generator.copy g]]
    | Id n -> [id n]
    | Comp (0, f, Id n) -> List.map (fun f -> f@(id n)) (create env f)
    | Comp (0, Id n, f) -> List.map (fun f -> (id n)@f) (create env f)
    | Comp (0,f,g) ->
      let f = create env f in
      let g = create env g in
      assert (List.length f = 1);
      assert (List.length g = 1);
      [(List.hd f)@(List.hd g)]
    | Comp (1,f,g) -> (create env f)@(create env g)
    | _ -> assert false

  let create env e : t =
    let ans = create env e in
    (* Set the vertical position *)
    List.iteri
      (fun i f ->
         List.iter (fun g -> g.G.y <- float_of_int i +. 0.5) f
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
      if G.rigid g then
        (
          assert (G.source g = 1 && G.target g = 1);
          g.G.target.(0) <- max g.G.source.(0) g.G.target.(0);
          g.G.source.(0) <- g.G.target.(0)
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
    val arc : t -> ?options:[`Middle_arrow of [`Left | `Right]] list -> float * float -> float * float -> float * float -> unit
    val disk : t -> float * float -> float * float -> unit
    val text : t -> float * float -> string -> unit
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

    let arc () ?(options=[]) (x,y) (rx,ry) (a,b) =
      let a = int_of_float a in
      let b = int_of_float b in
      Graphics.draw_arc (xscale x) (yscale y) (scale rx) (scale ry) a b

    let disk () = failwith "TODO"

    let text () = failwith "TODO"
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

    let arc oc ?(options=[]) (x,y) (rx,ry) (a,b) =
      let a = -.a in
      let b = -.b in
      (* The starting point is supposed to be horizontal for now... *)
      assert (int_of_float a mod 180 = 0);
      let o = ref [] in
      List.iter
        (function
          | `Middle_arrow `Right -> o := "middlearrow={>}" :: !o
          | `Middle_arrow `Left -> o := "middlearrow={<}" :: !o
        ) options;
      let o = String.concat "," !o in
      let o = if o = "" then "" else Printf.sprintf "[%s]" o in
      output_string oc (Printf.sprintf "\\draw%s ([shift=(%f:%f)]%f,%f) arc (%f:%f:%f and %f); " o a rx x y a b rx ry)

    let disk oc (x,y) (rx,ry) =
      output_string oc (Printf.sprintf "\\filldraw[fill=white] (%f,%f) ellipse (%f and %f); " x y rx ry)

    let text oc (x,y) s =
      output_string oc (Printf.sprintf "\\draw (%f,%f) node {$%s$}; " x y s)
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
      (* x-coordinate of the center *)
      let x =
        let x1 = if G.source g > 0 then Some (Float.mean g.G.source.(0) g.G.source.(G.source g - 1)) else None in
        let x2 = if G.target g > 0 then Some (Float.mean g.G.target.(0) g.G.target.(G.target g - 1)) else None in
        match x1, x2 with
        | Some x1, Some x2 -> Float.mean x1 x2
        | Some x1, None -> x1
        | None, Some x2 -> x2
        | None, None -> assert false
      in
      (* y-coordinate of the center *)
      let y = g.G.y in
      (* Draw wires. *)
      (
        if g.G.shape = `Cap then
          let options =
            match G.get g "arrow" with
            | "right" -> [`Middle_arrow `Right]
            | "left" -> [`Middle_arrow `Left]
            | _ -> []
          in
          if Array.length g.G.source = 2 then
            let l = g.G.source.(1) -. g.G.source.(0) in
            Draw.arc d ~options (x,y-.0.5) (l /. 2., 0.5) (-180.,0.)
          else
            let l = g.G.target.(1) -. g.G.target.(0) in
            Draw.arc d ~options (x,y+.0.5) (l /. 2., 0.5) (180.,0.)
        else
          let c = x in
          Array.iter (fun x -> Draw.line d (x,y-.0.5) (c,y)) g.G.source;
          Array.iter (fun x -> Draw.line d (c,y) (x,y+.0.5)) g.G.target;
      );
      (* Draw shape. *)
      (
        match g.G.shape with
        | `Circle ->
          let rx = G.get_float g "labelwidth" /. 2. in
          let ry = G.get_float g "labelheight" /. 2. in
          Draw.disk d (x,y) (rx,ry)
        | _ -> ()
      );
      (* Draw label. *)
      (
        if g.G.label <> "" then
          Draw.text d (x,y) g.G.label
      );
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
