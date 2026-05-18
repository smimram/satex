let horizontal = ref false

let tikzpicture_options () =
  if !horizontal then ["rotate=-90"]
  else ["yscale=-1"]
