open Extlib

(** Generators. *)
module Generator = struct
  type name = string

  (** A generator. *)
  type t =
    {
      options : (string * string) list; (** list of optional parameters and their value (a=b) *)
      shape : (** shape of the node *)
        [
          | `Circle (** traditional circled node *)
          | `Triangle
          | `Rectangle
          | `Merge of [`Left | `Right] (** the left / right wire is merged into the main one *)
          | `None (** no node decoration *)
          | `Cap (** special: a cap / cup *)
          | `Crossing
          | `Dots (** horizontal dots between two wires *)
          | `Label (** labels only *)
          | `Space (** a space *)
        ];
      source : float array; (** horizontal position of the source ports *)
      target : float array; (** horizontal position of the target ports *)
      mutable y : float; (** vertical position *)
    }

  let copy g =
    { g with 
      source = Array.copy g.source;
      target = Array.copy g.target;
    }

  let create source target options =
    let shape = ref `Circle in
    let label = ref "" in
    (* Normalize options. *)
    let options =
      List.map
        (function
          | "arrow","" -> "arrow","right"
          | "up","" | "u","" -> "position","0.2"
          | "down","" | "d","" -> "position","0.8"
          | "cap", ""
          | "cup", ""
          | "shape", "cup" -> "shape", "cap"
          | "triangle", "" -> "shape", "triangle"
          | "rectangle", "" -> "shape", "rectangle"
          | "blank", "" -> "shape", "blank"
          | "dots", "" -> "shape", "dots"
          | "crossing", "" -> "shape", "crossing"
          | "crossingr", "" -> "shape", "crossingr"
          | "crossingl", "" -> "shape", "crossingl"
          | "braid", "" -> "shape", "braid"
          | "braid'", "" -> "shape", "braid'"
          | "mergeleft", "" -> "shape", "mergeleft"
          | "mergeright", "" -> "shape", "mergeright"
          | l, _ when String.length l >= 2 && l.[0] = '"' && l.[String.length l - 1] = '"' ->
            "label", String.sub l 1 (String.length l - 2)
          | "ls",x -> "labelsize",x
          | "bordercolor",x -> "labelbordercolor",x
          | "fill",x -> "labelcolor",x
          | lv -> lv
        ) options
    in
    let options =
      List.map
        (function
          | "labelsize", x -> ["labelwidth", x; "labelheight", x]
          | "shape", "blank" -> ["shape", "rectangle"; "labelbordercolor", "white"]
          | "shape", "dots" -> ["shape", "dots"; "label", "\\ldots"]
          | "shape", "crossingr" -> ["shape", "crossing"; "kind", "right"]
          | "shape", "crossingl" -> ["shape", "crossing"; "kind", "left"]
          | "shape", "braid" -> ["shape", "crossing"; "kind", "braid"]
          | "shape", "braid'" -> ["shape", "crossing"; "kind", "braid'"]
          | lv -> [lv]
        ) options |> List.flatten
    in
    (* Set default options. *)
    let options = options@["arrow", "none"; "position", "0.5"] in
    let options = options@(if List.mem_assoc "label" options then ["labelwidth", ".5"; "labelheight", ".5"] else ["labelwidth", ".2"; "labelheight", ".2"]) in
    (* Parse options. *)
    List.iter_right
      (function
        | "shape", "none" ->
          assert ((source = 1 && target = 1) || (source = 2 && target = 2)); shape := `None
        | "shape", "cap" ->
          assert ((source = 2 && target <= 1) || (source <= 1 && target = 2));
          shape := `Cap
        | "shape", "label" ->
          shape := `Label
        | "shape", "triangle" ->
          assert (source = 1 || target = 1);
          shape := `Triangle
        | "shape", "rectangle" ->
          assert (source > 0 || target > 0);
          shape := `Rectangle
        | "shape", "dots" ->
          assert (source = 2 && target = 2);
          shape := `Dots
        | "shape", "crossing" ->
          assert (source = target && source >= 2);
          shape := `Crossing
        | "shape", "space" ->
          shape := `Space
        | "shape", "mergeleft" ->
          assert ((source = 2 && target = 1) || (source = 1 && target = 2));
          shape := `Merge `Left
        | "shape", "mergeright" ->
          assert ((source = 2 && target = 1) || (source = 1 && target = 2));
          shape := `Merge `Right
        | "label", l ->
          label := l
        | l, v -> ()
      ) options;
    (* Printf.printf "options for %s: %s\n%!" name (String.concat ", " (List.map (fun (l,v) -> l^"="^v) options)); *)
    {
      options;
      shape = !shape;
      source = Array.init source (fun _ -> -1.);
      target = Array.init target (fun _ -> -1.);
      y = 0.;
    }

  let source g = Array.length g.source

  let target g = Array.length g.target

  let shape g = g.shape

  let id () = create 1 1 ["name", "1"; "shape","none"]

  let get g o = List.assoc o g.options

  let get_float g o = float_of_string (get g o)

  let name g = get g "name"

  let label g = try get g "label" with Not_found -> ""

  let label_width g = try get_float g "labelwidth" with Not_found -> 0.5

  let label_height g = try get_float g "labelheight" with Not_found -> 0.5

  let get_source g i = g.source.(i)

  let get_target g i = g.target.(i)

  let set_source g i x =
    if x > get_source g i then Printf.printf "update source %d of %s: %f -> %f\n%!" i (name g) (get_source g i) x;
    g.source.(i) <- max g.source.(i) x

  let set_target g i x =
    if x > get_target g i then Printf.printf "update target %d of %s: %f -> %f\n%!" i (name g) (get_target g i) x;
    g.target.(i) <- max g.target.(i) x
end

module G = Generator

(** Expression for a cell. *)
type expr =
  | Gen of G.t (** a generator *)
  | Id of int (** an object *)
  | Comp of int * expr * expr (** composite in given dimension *)

let rec string_of_expr ?(p=false) = function
  | Gen g -> Printf.sprintf "(%d->%d)" (G.source g) (G.target g)
  | Id n -> string_of_int n
  | Comp (n, f, g) ->
    Printf.sprintf "%s%s *%d %s%s"
      (if p then "(" else "")
      (string_of_expr ~p:true f)
      n
      (string_of_expr ~p:true g)
      (if p then ")" else "")

(** Cell declarations (numbered in order to be able to identify them in
    LaTeX). The list contains the TikZ options for the cell. *)
type t = (int * string list * expr) list

(** Typing error. *)
exception Typing of string

(** Type of a cell. *)
let rec typ = function
  | Gen g -> G.source g, G.target g
  | Id n -> n, n
  | Comp (n, e1, e2) ->
    let s1, t1 = typ e1 in
    let s2, t2 = typ e2 in
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

  let create e : t =
    let rec aux e =
      let id n =
        if n = 0 then [G.create 0 0 ["name", "0"; "shape", "space"; "width", "0"]]
        else List.init n (fun _ -> G.id ())
      in
      match e with
      | Gen g -> [[Generator.copy g]]
      | Id n -> [id n]
      | Comp (0, f, Id n) -> List.map (fun f -> f@(id n)) (aux f)
      | Comp (0, Id n, f) -> List.map (fun f -> (id n)@f) (aux f)
      | Comp (0,f,g) ->
        let f = aux f in
        let g = aux g in
        assert (List.length f = 1);
        assert (List.length g = 1);
        [(List.hd f)@(List.hd g)]
      | Comp (1,f,g) -> (aux f)@(aux g)
      | _ -> assert false
    in
    let ans = aux e in
    (* Set the vertical position *)
    List.iteri
      (fun i f ->
         List.iter (fun g -> g.G.y <- float_of_int i +. 0.5) f
      ) ans;
    ans

  let copy : t -> t = List.map (List.map G.copy)

  let sources (f:t) =
    List.map (fun g -> g.G.source) (List.hd f)

  let targets (f:t) =
    List.map (fun g -> g.G.target) (List.last f)

  let typeset (f:t) =
    let last_source = ref (-1.) in
    let last_target = ref (-1.) in
    let generator g =
      (* Space up the sources. *)
      if G.source g > 0 then
        (
          G.set_source g 0 (!last_source +. 1.);
          last_source := G.get_source g 0
        );
      for i = 0 to Array.length g.G.source - 2 do
        G.set_source g (i+1) (G.get_source g i +. 1.);
        last_source := G.get_source g (i+1)
      done;
      if G.shape g = `Space then last_source := !last_source +. G.get_float g "width";
      (* Space up the targets. *)
      if G.target g > 0 then
        (
          G.set_target g 0 (!last_target +. 1.);
          last_target := G.get_target g 0
        );
      for i = 0 to Array.length g.G.target - 2 do
        G.set_target g (i+1) (G.get_target g i +. 1.);
        last_target := G.get_target g (i+1)
      done;
      if G.shape g = `Space then last_target := !last_target +. G.get_float g "width";
      (* Enforce propagation for nullary operations. *)
      if G.source g = 0 then last_source := max !last_source !last_target;
      if G.target g = 0 then last_target := max !last_target !last_source;
      (* Propagate down and up. *)
      if G.shape g = `Label || G.shape g = `Dots || G.shape g = `Crossing then
        (
          (* For labels we pairwaise align sources. *)
          assert (G.source g = G.target g);
          for i = 0 to G.source g - 1 do
            G.set_target g i (G.get_source g i);
            G.set_source g i (G.get_target g i)
          done
        )
      else if G.shape g = `Merge `Left then
        (
          if G.source g = 2 then
            (
              G.set_target g 0 (G.get_source g 1);
              G.set_source g 1 (G.get_target g 0)
            )
          else
            (
              G.set_target g 1 (G.get_source g 0);
              G.set_source g 0 (G.get_target g 1)
            )
              
        )
      else if G.shape g = `Merge `Right then
        (
          G.set_target g 0 (G.get_source g 0);
          G.set_source g 0 (G.get_target g 0)       
        )
      else if G.source g = 1 && G.target g = 1 then
        (
          G.set_target g 0 (G.get_source g 0);
          G.set_source g 0 (G.get_target g 0)
        )
      else if G.target g = 1 && G.source g > 0 then
        (
          G.set_target g 0 ((G.get_source g 0 +. G.get_source g (G.source g -1)) /. 2.);
          G.set_source g (G.source g-1) (2. *. G.get_target g 0 -. G.get_source g 0)
        )
      else if G.source g = 1 && G.target g > 0 then
        (
          G.set_source g 0 ((G.get_target g 0 +. G.get_target g (G.target g-1)) /. 2.);
          G.set_target g (G.target g-1) (2. *. G.get_source g 0 -. G.get_target g 0)
        )
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
    let prev = ref [] in
    let f = ref f in
    let i = ref 0 in
    (* Iterate until we reach a fixpoint. *)
    while !f <> !prev && !i <= 100 do
      incr i;
      Printf.printf "round %d\n%!" !i;
      prev := copy !f;
      stack !f
    done

  module Draw = struct
    type t = out_channel

    let create fname id options =
      let options = String.concat "," options in
      let oc = open_out_gen [Open_creat; Open_append] 0o644 fname in
      output_string oc (Printf.sprintf "\\defsatexfig{%d}{\n  \\begin{tikzpicture}[baseline=(current bounding box.center),yscale=-1,scale=0.5,every path/.style={join=round,cap=round},%s]\n" id options);
      oc

    let close oc =
      output_string oc "  \\end{tikzpicture}\n}\n";
      close_out oc

    let line oc ?(options=[]) (x1,y1) (x2,y2) =
      let options =
        List.map
          (function
            | `Color c -> c
            | `Thick 2 -> "very thick"
            | `Thick 3 -> "ultra thick"
            | `Thick _ -> "thick"
            | `Width w -> "line width="^w
            | `Phantom -> "opacity=0."
          ) options
        |> String.concat ","
      in
      let options = if options = "" then "" else Printf.sprintf "[%s]" options in
      output_string oc (Printf.sprintf "    \\draw%s (%f,%f) -- (%f,%f);\n" options x1 y1 x2 y2)

    let arc oc ?(options=[]) (x,y) (rx,ry) (a,b) =
      let a = -.a in
      let b = -.b in
      (* The starting point is supposed to be horizontal for now... *)
      assert (int_of_float a mod 180 = 0);
      let options =
        List.map
          (function
            | `Middle_arrow `Right -> "middlearrow={>}"
            | `Middle_arrow `Left -> "middlearrow={<}"
          ) options
        |> String.concat ","
      in
      let options = if options = "" then "" else Printf.sprintf "[%s]" options in
      output_string oc (Printf.sprintf "    \\draw%s ([shift=(%f:%f)]%f,%f) arc (%f:%f:%f and %f);\n" options a rx x y a b rx ry)

    let polygon oc ?(options=[]) p =
      let options =
        List.map
          (function
            | `Rounded_corners -> "rounded corners=1pt"
            | `Color c -> c
            | `Fill c -> "fill="^c
          ) options
        |> String.concat ","
      in
      let p = p |> List.map (fun (x,y) -> Printf.sprintf "(%f,%f)" x y) |> String.concat " -- " in
      output_string oc (Printf.sprintf "    \\filldraw[fill=white,%s] %s -- cycle;\n" options p)

    let disk oc ?(options=[]) (x,y) (rx,ry) =
      let options =
        List.map
          (function
            | `Color c -> c
            | `Fill c -> "fill="^c
          ) options
        |> String.concat ","
      in
      output_string oc (Printf.sprintf "    \\filldraw[fill=white,%s] (%f,%f) ellipse (%f and %f);\n" options x y rx ry)

    let text oc (x,y) s =
      output_string oc (Printf.sprintf "    \\draw (%f,%f) node {$%s$};\n" x y s)
  end

  let draw fname id options f =
    let d = Draw.create fname id options in
    let draw_generator g =
      (* x-coordinate of the center *)
      let x =
        let x1 = if G.source g > 0 then Some (g.G.source.(0), g.G.source.(G.source g - 1)) else None in
        let x2 = if G.target g > 0 then Some (g.G.target.(0), g.G.target.(G.target g - 1)) else None in
        match x1, x2 with
        | Some (x1,x1'), Some (x2,x2') -> Float.mean (min x1 x2) (max x2 x2')
        | Some (x1,x1'), None -> Float.mean x1 x1'
        | None, Some (x2,x2') -> Float.mean x2 x2'
        | None, None -> assert (G.shape g = `Space); 0.
      in
      (* y-coordinate of the center *)
      let y = g.G.y in
      (* Draw wires. *)
      (
        if G.shape g = `Cap then
          let options =
            match G.get g "arrow" with
            | "right" -> [`Middle_arrow `Right]
            | "left"  -> [`Middle_arrow `Left]
            | _ -> []
          in
          if G.source g = 2 then
            (
              let l = g.G.source.(1) -. g.G.source.(0) in
              Draw.arc d ~options (x,y-.0.5) (l /. 2., 0.5) (-180.,0.);
              if G.target g = 1 then Draw.line d (x,y) (x,y+.0.5)
            )
          else
            (
              let l = g.G.target.(1) -. g.G.target.(0) in
              Draw.arc d ~options (x,y+.0.5) (l /. 2., 0.5) (180.,0.);
              if G.source g = 1 then Draw.line d (x,y-.0.5) (x,y)
            )
        else if G.shape g = `Label then ()
        else if G.shape g = `Triangle || G.shape g = `Rectangle then
          (
            Array.iter (fun x -> Draw.line d (x,y-.0.5) (x,y-.0.25)) g.G.source;
            Array.iter (fun x -> Draw.line d (x,y+.0.25) (x,y+.0.5)) g.G.target;
          )
        else if G.shape g = `Merge `Left || G.shape g = `Merge `Right then
          (
            let x1, x2 =
              if G.source g = 2 then G.get_source g 0, G.get_source g 1
              else G.get_target g 0, G.get_target g 1
            in
            let x1, x2 = if G.shape g = `Merge `Left then x1, x2 else x2, x1 in
            Draw.line d (x2,y-.0.5) (x2,y+.0.5);
            if G.source g = 2 then Draw.arc d (x2,y-.0.5) (x2-.x1,0.5) (180.,270.)
            else Draw.arc d (x2,y+.0.5) (x2-.x1,0.5) (180.,90.)
          )
        else if G.shape g = `Space && G.get_float g "width" = 0. then
          (
            (* Take some vertical space. *)
            Draw.line d ~options:[`Phantom] (0.,y-.0.5) (0.,y+.0.5)
          )
        else if G.shape g = `Crossing then
          (
            let kind = try G.get g "kind" with Not_found -> "crossing" in
            let x = g.G.source in
            let n = Array.length x in
            if kind = "braid" || kind = "right" then
              (
                for i = 1 to n-1 do
                  Draw.line d (x.(i),y-.0.5) (x.(i-1),y+.0.5);
                  let a = float_of_int i /. float_of_int n in
                  if kind = "braid" then Draw.disk d ~options:[`Color "white"] (x.(0)+.(x.(n-1)-.x.(0))*.a,y-.0.5+.a) (0.1,0.1);
                done;
                Draw.line d (x.(0),y-.0.5) (x.(n-1),y+.0.5)
              )
            else if kind = "braid'" || kind = "left" then
              (
                for i = 0 to n-2 do
                  Draw.line d (x.(i),y-.0.5) (x.(i+1),y+.0.5);
                  let a = float_of_int (i+1) /. float_of_int n in
                  if kind = "braid'" then Draw.disk d ~options:[`Color "white"] (x.(0)+.(x.(n-1)-.x.(0))*.a,y+.0.5-.a) (0.1,0.1);
                done;
                Draw.line d (x.(n-1),y-.0.5) (x.(0),y+.0.5)
              )
            else
              (
                for i = 0 to n-1 do
                  Draw.line d (x.(i),y-.0.5) (x.(n-1-i),y+.0.5);
                done
              )
          )
        else if G.shape g = `Dots then
          (
            Array.iter2 (fun x x' -> assert (x = x'); Draw.line d (x,y-.0.5) (x,y+.0.5)) g.G.source g.G.target
          )
        else
          (
            Array.iter (fun x' -> Draw.line d (x',y-.0.5) (x,y)) g.G.source;
            Array.iter (fun x' -> Draw.line d (x,y) (x',y+.0.5)) g.G.target;
          )
      );
      (* Draw shape. *)
      (
        let options =
          (try [`Color (List.assoc "labelbordercolor" g.G.options)] with Not_found -> [])@
          (try [`Fill (List.assoc "labelcolor" g.G.options)] with Not_found -> [])
        in
        match G.shape g with
        | `Circle ->
          let rx = G.label_width g /. 2. in
          let ry = G.label_height g /. 2. in
          Draw.disk d ~options (x,y) (rx,ry)
        | `Triangle ->
          if G.target g = 1 then
            Draw.polygon d ~options [G.get_source g 0,y-.0.25; G.get_source g (G.source g-1), y-.0.25; G.get_target g 0, y+.0.25]
          else
            Draw.polygon d ~options [G.get_target g 0,y+.0.25; G.get_target g (G.target g-1), y+.0.25; G.get_source g 0, y-.0.25]
        | `Rectangle ->
          let x1 =
            min
              (if G.source g > 0 then G.get_source g 0 else Float.infinity)
              (if G.target g > 0 then G.get_target g 0 else Float.infinity)
          in
          let x2 =
            max
              (if G.source g > 0 then G.get_source g (G.source g - 1) else Float.neg_infinity)
              (if G.target g > 0 then G.get_target g (G.target g - 1) else Float.neg_infinity)
          in
          let x1 = x1 -. 0.25 in
          let x2 = x2 +. 0.25 in
          let y1 = y -. 0.25 in
          let y2 = y +. 0.25 in
          let options = `Rounded_corners::options in
          Draw.polygon d ~options [x1,y1; x2,y1; x2,y2; x1,y2]
        | _ -> ()
      );
      (* Draw label. *)
      (
        if G.shape g = `Label then
          let label = List.find_all (fun (l,_) -> l = "label") g.G.options |> List.map snd |> List.rev |> Array.of_list in
          for i = 0 to G.source g - 1 do
            let x = g.G.source.(i) in
            let y = y -. 0.5 +. G.get_float g "position" in
            Draw.text d (x,y) label.(i)
          done
        else if G.label g <> "" then
          Draw.text d (x,y) (G.label g)
      );
    in
    List.iter (List.iter draw_generator) f;
    Draw.close d
end

let draw fname cells =
  (try Sys.remove fname with _ -> ());
  List.iter
    (fun (id,o,e) ->
       let f = Stack.create e in
       Stack.typeset f;
       Stack.draw fname id o f
    ) cells
