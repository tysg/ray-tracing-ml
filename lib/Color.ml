open Vec3

let write_color pixel samples_per_pixel =
  let clamp x min max = if x < min then min else if x > max then max else x in
  let pixel =
    map_vec3 pixel (fun p ->
        256. *. clamp (sqrt 1.0 /. Float.of_int samples_per_pixel *. p) 0. 0.999 )
  in
  let to_string f = string_of_int (Int.of_float f) in
  to_string pixel.x ^ " " ^ to_string pixel.y ^ " " ^ to_string pixel.z ^ "\n"


let white = { x = 1.0; y = 1.0; z = 1.0 }

let light_blue = { x = 0.5; y = 0.7; z = 1.0 }

let red = { x = 1.; y = 0.; z = 0. }