open Vec3

let write_color pixel =
  let n = 255.999 in
  let to_string f = string_of_int (Int.of_float (n *. f)) in
  to_string pixel.x ^ " " ^ to_string pixel.y ^ " " ^ to_string pixel.z ^ "\n"

let white = { x = 1.0; y = 1.0; z = 1.0 }

let light_blue = { x = 0.5; y = 0.7; z = 1.0 }

let red = { x = 1.; y = 0.; z = 0. }