module Array = struct
  include Array

  let last a =
    a.(Array.length a - 1)
end

module List = struct
  include List

  let rec iter_right f = function
    | [] -> ()
    | x::l -> iter_right f l; f x

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
