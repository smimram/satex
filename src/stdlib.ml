module Option = struct
  let default x = function
    | Some x -> x
    | None -> x
end
