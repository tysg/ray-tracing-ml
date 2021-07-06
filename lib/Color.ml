open Vec3

type color = vec3

let write_color pixel =
  let n = 255.999 in
  let to_string f = string_of_int (Int.of_float (n *. f)) in
  to_string pixel.x ^ " " ^ to_string pixel.y ^ " " ^ to_string pixel.z ^ "\n"


