open Extlib

exception Error of (Lexing.position * Lexing.position) option * string

let error ?pos e = raise (Error (pos, e))

(** Generators. *)
module Generator = struct
  type name = string

  (** A generator. *)
  type t =
    {
      options : (string * string) list; (** list of optional parameters and their value (a=b) *)
      shape :
        [
          | `Id (** Identity *)
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
        ]; (** shape of the node *)
      source : float array; (** horizontal position of the source ports *)
      target : float array; (** horizontal position of the target ports *)
      mutable x : float; (** horizontal position (for operators without inputs nor outputs) *)
      mutable y : float; (** vertical position *)
      mutable height: float; (** height *)
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
          | "up","" | "u","" -> "position","0.3"
          | "down","" | "d","" -> "position","0.7"
          | "right","" -> "offset", "0.5"
          | "cap", ""
          | "cup", ""
          | "shape", "cup" -> "shape", "cap"
          | "triangle", "" -> "shape", "triangle"
          | "rectangle", "" -> "shape", "rectangle"
          | "r", "" -> "shape", "rectangle"
          | "circle", "" -> "shape", "circle"
          | "c", "" -> "shape", "circle"
          | "lefthalfcircle", "" -> "shape", "lefthalfcircle"
          | "righthalfcircle", "" -> "shape", "righthalfcircle"
          | "blank", "" -> "shape", "blank"
          | "dots", "" -> "shape", "dots"
          | "id", "" -> "shape", "id"
          | "crossing", "" -> "shape", "crossing"
          | "crossingr", "" -> "shape", "crossingr"
          | "crossingl", "" -> "shape", "crossingl"
          | "crossing'", "" -> "shape", "crossing'"
          | "crossingr'", "" -> "shape", "crossingr'"
          | "crossingl'", "" -> "shape", "crossingl'"
          | "braid", "" -> "shape", "braidr"
          | "shape", "braid" -> "shape", "braidr"
          | "braidr", "" -> "shape", "braidr"
          | "braidl", "" -> "shape", "braidl"
          | "braid'", "" -> "shape", "braidr'"
          | "braidr'", "" -> "shape", "braidr'"
          | "braidl'", "" -> "shape", "braidl'"
          | "mergeleft", "" -> "shape", "mergeleft"
          | "mergeright", "" -> "shape", "mergeright"
          | l, _ when String.length l >= 2 && l.[0] = '"' && l.[String.length l - 1] = '"' ->
            "label", String.sub l 1 (String.length l - 2)
          | "ls",x -> "labelsize",x
          | "size",x -> "labelsize",x
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
          | "shape", "dots" -> ["shape", "dots"; "label", "\\ldots"; "height", "0"; "width", "1"]
          | "shape", "crossingr" -> ["shape", "crossing"; "kind", "right"]
          | "shape", "crossingl" -> ["shape", "crossing"; "kind", "left"]
          | "shape", "braidr" -> ["shape", "crossing"; "kind", "braidr"]
          | "shape", "braidl" -> ["shape", "crossing"; "kind", "braidl"]
          | "shape", "crossing'" -> ["shape", "crossing"; "kind", "crossing'"]
          | "shape", "crossingr'" -> ["shape", "crossing"; "kind", "right'"]
          | "shape", "crossingl'" -> ["shape", "crossing"; "kind", "left'"]
          | "shape", "braidr'" -> ["shape", "crossing"; "kind", "braidr'"]
          | "shape", "braidl'" -> ["shape", "crossing"; "kind", "braidl'"]
          | "shape", "lefthalfcircle" -> ["shape", "circle"; "kind", "lefthalf"]
          | "shape", "righthalfcircle" -> ["shape", "circle"; "kind", "righthalf"]
          | lv -> [lv]
        ) options |> List.flatten
    in
    (* Set default options. *)
    let options = options@["arrow", "none"; "position", "0.5"; "offset", "0"] in
    (* Parse options. *)
    List.iter_right
      (function
        | "shape", "none" ->
          assert ((source = 1 && target = 1) || (source = 2 && target = 2)); shape := `None
        | "shape", "id" ->
          assert (source = target); shape := `Id
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
        | _ -> ()
      ) options;
    (* Some more shape-specific hacks. *)
    let options = options@(if !shape = `Circle && not (List.mem_assoc "label" options) then ["labelwidth", ".3"; "labelheight", ".3"] else ["labelwidth", ".6"; "labelheight", ".6"]) in
    let options =
    List.map
      (function
        | "width", x when !shape <> `Space && !shape <> `Dots -> "labelwidth", x
        | lv -> lv
      ) options
    in
    (* Printf.printf "options for %s: %s\n%!" name (String.concat ", " (List.map (fun (l,v) -> l^"="^v) options)); *)
    {
      options;
      shape = !shape;
      source = Array.init source (fun _ -> -1.);
      target = Array.init target (fun _ -> -1.);
      x = 0.;
      y = 0.;
      height = 0.;
    }

  let source g = Array.length g.source

  let target g = Array.length g.target

  let shape g = g.shape

  let id () = create 1 1 ["name", "1"; "shape", "none"]

  let get g o = List.assoc o g.options

  let get_float g o = float_of_string (get g o)

  let name g = get g "name"

  let label g = try get g "label" with Not_found -> ""

  let label_width g = try get_float g "labelwidth" with Not_found -> 0.5

  let label_height g = try get_float g "labelheight" with Not_found -> 0.5

  let get_source g i = g.source.(i)

  let get_target g i = g.target.(i)

  let set_source g i x =
    (* if x > get_source g i then Printf.printf "update source %d of %s: %f -> %f\n%!" i (name g) (get_source g i) x; *)
    g.source.(i) <- max g.source.(i) x

  let set_target g i x =
    (* if x > get_target g i then Printf.printf "update target %d of %s: %f -> %f\n%!" i (name g) (get_target g i) x; *)
    g.target.(i) <- max g.target.(i) x
end

module G = Generator

(** Expression for a cell. *)
type expr =
  | Gen of G.t (** a generator *)
  | Comp of int * expr * expr (** composite in given dimension *)

let rec string_of_expr ?(p=false) = function
  | Gen g -> Printf.sprintf "(%d->%d)" (G.source g) (G.target g)
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

(** Normalized form for expressions (as a stack of slices). *)
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
      match e with
      | Gen g -> [[Generator.copy g]]
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
    (* Set height and vertical position. *)
    let y = ref 0. in
    List.iter
      (fun f ->
         let height g =
           try Some (G.get_float g "height")
           with Not_found ->
             if G.shape g = `Id then None (* Identities can have null height *)
             else if G.shape g = `Space then Some 0.
             else Some 1.
         in
         let max h h' =
           match h, h' with
             | None, Some h
             | Some h, None -> Some h
             | Some h, Some h' -> Some (max h h')
             | None, None -> None
         in
         let h = List.fold_left (fun h g -> max h (height g)) None f in
         let h = match h with Some h -> h | None -> 1. in
         List.iter
           (fun g ->
              g.G.height <- h;
              g.G.y <- !y +. h /. 2.
           ) f;
         y := !y +. h
      ) ans;
    ans

  let copy : t -> t = List.map (List.map G.copy)

  let sources (f:t) =
    List.map (fun g -> g.G.source) (List.hd f)

  let targets (f:t) =
    List.map (fun g -> g.G.target) (List.last f)

  (** Repeatedly move things on the right in order for them not to overlap. *)
  let typeset (f:t) =
    (* Last seen rightmost source. *)
    let last_source = ref (-1.) in
    (* Last seen rightmost target. *)
    let last_target = ref (-1.) in
    (* Typeset a generators by moving its input and output wires. *)
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
      if G.shape g = `Dots then G.set_source g 1 (G.get_source g 0 +. G.get_float g "width");
      if G.shape g = `Space then
        (
          let width = G.get_float g "width" in
          g.G.x <- !last_source +. width /. 2. +. 0.5;
          last_source := !last_source +. width
        );
      if G.shape g <> `Space && G.shape g <> `Label && G.source g = 0 && G.target g = 0 then
        (
          g.G.x <- max g.G.x (!last_source +. 1.);
          last_source := !last_source +. 1.
        );
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
      (* Ensure that next operator is not too close of a box. *)
      if G.shape g = `Rectangle then (last_source := max !last_source !last_target; last_target := !last_source);
      (* Propagate down and up. *)
      if G.shape g = `Id || G.shape g = `Label || G.shape g = `Dots || G.shape g = `Crossing then
        (
          (* Pairwaise align sources and targets. *)
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
          G.set_target g 0 ((G.get_source g 0 +. G.get_source g (G.source g - 1)) /. 2.);
          G.set_source g (G.source g - 1) (2. *. G.get_target g 0 -. G.get_source g 0)
        )
      else if G.source g = 1 && G.target g > 0 then
        (
          G.set_source g 0 ((G.get_target g 0 +. G.get_target g (G.target g - 1)) /. 2.);
          G.set_target g (G.target g - 1) (2. *. G.get_source g 0 -. G.get_target g 0)
        )
    in
    (* Typeset a slice. *)
    let slice f =
      last_source := (-1.);
      last_target := (-1.);
      List.iter generator f;
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
      Printf.printf "round %d\r%!" !i;
      prev := copy !f;
      stack !f
    done

  (** Abstract basic drawing operations. *)
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
      if (x1,y1) <> (x2,y2) || List.mem `Phantom options then
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
      assert (int_of_float a mod 90 = 0);
      let options =
        List.map
          (function
            | `Middle_arrow `Right -> "middlearrow={>}"
            | `Middle_arrow `Left -> "middlearrow={<}"
            | `Color c -> c
            | `Fill c -> "fill="^c
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
      output_string oc (Printf.sprintf "    \\draw (%f,%f) node {$\\scriptstyle %s$};\n" x y s)
  end

  (** Draw morphism. *)
  let draw fname id options (f:t) =
    let d = Draw.create fname id options in
    let draw_generator g =
      (* x-coordinate of the center *)
      let x =
        let x1 = if G.source g > 0 then Some (g.G.source.(0), g.G.source.(G.source g - 1)) else None in
        let x2 = if G.target g > 0 then Some (g.G.target.(0), g.G.target.(G.target g - 1)) else None in
        match x1, x2 with
        | Some (x1,x1'), Some (x2,x2') -> Float.mean (min x1 x2) (max x1' x2')
        | Some (x1,x1'), None -> Float.mean x1 x1'
        | None, Some (x2,x2') -> Float.mean x2 x2'
        | None, None -> g.G.x
      in
      (* y-coordinate of the center *)
      let y = g.G.y in
      let h = g.G.height in
      (* Draw wires. *)
      (
        match G.shape g with
        | `Id | `Dots ->
          let x = g.G.source in
          let n = Array.length x in
          assert (Array.length g.G.source = Array.length g.G.target);
          if n = 0 then
            Draw.line d ~options:[`Phantom] (0.,y-.h/.2.) (0.,y+.h/.2.)
          else
            for i = 0 to n - 1 do
              Draw.line d (x.(i),y-.h/.2.) (x.(i),y+.h/.2.)
            done
        | `Cap ->
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
        | `Label -> ()
        | `Triangle | `Rectangle ->
          let lh = G.label_height g in
          Array.iter (fun x -> Draw.line d (x,y-.h/.2.) (x,y-.lh/.2.)) g.G.source;
          Array.iter (fun x -> Draw.line d (x,y+.lh/.2.) (x,y+.h/.2.)) g.G.target;
        | `Merge _ ->
            let x1, x2 =
              if G.source g = 2 then G.get_source g 0, G.get_source g 1
              else G.get_target g 0, G.get_target g 1
            in
            let x1, x2 = if G.shape g = `Merge `Left then x1, x2 else x2, x1 in
            Draw.line d (x2,y-.0.5) (x2,y+.0.5);
            if G.source g = 2 then Draw.arc d (x2,y-.0.5) (x2-.x1,0.5) (180.,270.)
            else Draw.arc d (x2,y+.0.5) (x2-.x1,0.5) (180.,90.)
        | `Space when G.get_float g "width" = 0. ->
          (* Take some vertical space. *)
          Draw.line d ~options:[`Phantom] (0.,y-.0.5) (0.,y+.0.5)
        | `Crossing ->
          let kind = try G.get g "kind" with Not_found -> "crossing" in
          let kind, variant =
            (* do we have a variant starting with straigt lines *)
            if kind.[String.length kind-1] = '\'' then
              String.sub kind 0 (String.length kind - 1), true
            else
              kind, false
          in
          let x = g.G.source in
          let n = Array.length x in
          let dy =
            if variant then
              (
                for i = 0 to n-1 do
                  Draw.line d (x.(i),y-.0.5) (x.(i),y-.0.25);
                  Draw.line d (x.(i),y+.0.25) (x.(i),y+.0.5);
                done;
                0.25
              )
            else 0.5
          in
          if kind = "braidr" || kind = "right" then
            (
              for i = 1 to n-1 do
                Draw.line d (x.(i),y-.dy) (x.(i-1),y+.dy);
                let a = float_of_int i /. float_of_int n in
                if kind = "braidr" then Draw.disk d ~options:[`Color "white"] (x.(0)+.(x.(n-1)-.x.(0))*.a,y-.dy+.(a*.2.*.dy)) (0.1,0.1);
              done;
              Draw.line d (x.(0),y-.dy) (x.(n-1),y+.dy)
            )
          else if kind = "braidl" || kind = "left" then
            (
              for i = 0 to n-2 do
                Draw.line d (x.(i),y-.dy) (x.(i+1),y+.dy);
                let a = float_of_int (i+1) /. float_of_int n in
                if kind = "braidl" then Draw.disk d ~options:[`Color "white"] (x.(0)+.(x.(n-1)-.x.(0))*.a,y+.dy-.a*.2.*.dy) (0.1,0.1);
              done;
              Draw.line d (x.(n-1),y-.dy) (x.(0),y+.dy)
            )
          else
            (
              for i = 0 to n-1 do
                Draw.line d (x.(i),y-.dy) (x.(n-1-i),y+.dy);
              done
            )
        | _ ->
          Array.iter (fun x' -> Draw.line d (x',y-.h/.2.) (x,y)) g.G.source;
          Array.iter (fun x' -> Draw.line d (x,y) (x',y+.h/.2.)) g.G.target;
      );
      (* Draw shape. *)
      (
        let options =
          (try [`Color (G.get g "labelbordercolor")] with Not_found -> [])@
          (try [`Fill (G.get g "labelcolor")] with Not_found -> [])
        in
        match G.shape g with
        | `Circle ->
          let rx = G.label_width g /. 2. in
          let ry = G.label_height g /. 2. in
          (
            match (try G.get g "kind" with _ -> "") with
            | "lefthalf" ->
              Draw.arc d ~options (x,y) (rx,ry) (90.,270.);
              Draw.line d (x,y-.ry) (x,y+.ry)
            | "righthalf" ->
              Draw.arc d ~options (x,y) (rx,ry) (-90.,90.);
              Draw.line d (x,y-.ry) (x,y+.ry)
            | _ -> Draw.disk d ~options (x,y) (rx,ry)
          )
        | `Triangle ->
          let h = G.label_height g in
          if G.target g = 1 then
            Draw.polygon d ~options [G.get_source g 0,y-.h/.2.; G.get_source g (G.source g-1), y-.h/.2.; G.get_target g 0, y+.h/.2.]
          else
            Draw.polygon d ~options [G.get_target g 0,y+.h/.2.; G.get_target g (G.target g-1), y+.h/.2.; G.get_source g 0, y-.h/.2.]
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
          let w = G.label_width g in
          let h = G.label_height g in
          let x1 = x1 -. w /. 2. in
          let x2 = x2 +. w /. 2. in
          let y1 = y -. h /. 2. in
          let y2 = y +. h /. 2. in
          let options = `Rounded_corners::options in
          Draw.polygon d ~options [x1,y1; x2,y1; x2,y2; x1,y2]
        | _ -> ()
      );
      (* Draw label. *)
      (
        if G.shape g = `Label then
          let label = List.find_all (fun (l,_) -> l = "label") g.G.options |> List.map snd |> List.rev |> Array.of_list in
          for i = 0 to G.source g - 1 do
            let x = g.G.source.(i) +. G.get_float g "offset" in
            let y = y +. h *. (G.get_float g "position" -. 0.5) in
            Draw.text d (x,y) label.(i)
          done
        else if G.label g <> "" then
          let h = G.label_height g in
          let y = y +. h *. (G.get_float g "position" -. 0.5) in
          Draw.text d (x,y) (G.label g)
      );
      (* Ensure that bounding box is correct. *)
      (
        if G.shape g <> `Space && G.shape g <> `Label then
          (
            if G.source g = 0 then
              Draw.line d ~options:[`Phantom] (x,y-.h/.2.) (x,y-.h/.2.);
            if G.target g = 0 then
              Draw.line d ~options:[`Phantom] (x,y+.h/.2.) (x,y+.h/.2.)
          )
      )
    in
    List.iter (List.iter draw_generator) f;
    Draw.close d
end

let global_options = ref []
let add_global_option o = global_options := o :: !global_options

let draw fname cells =
  (try Sys.remove fname with _ -> ());
  let drawn = ref [] in
  List.iter
    (fun (id,o,e) ->
       if List.mem id !drawn then
         (* We sometimes have duplicates in the satex file, ignore them for now. *)
         Printf.printf "figure %d already drawn, ignoring\n%!" id
       else
         let f = Stack.create e in
         Stack.typeset f;
         let o = o @ !global_options in
         Stack.draw fname id o f;
         Printf.printf "figure %d drawn\n%!" id;
         drawn := id :: !drawn
    ) cells
