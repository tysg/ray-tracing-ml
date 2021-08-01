open Vec3

type t =
  { origin : Vec3.t
  ; direction : Vec3.t
  }

type facing_direction =
  | Front
  | Back

let create origin direction = { origin; direction }

let at r t = r.origin +| (r.direction */ t)

let ( @/ ) r t = at r t

let find_facing_direction ray normal =
  if dot ray.direction normal < 0. then Front else Back


let refract uv n etai_over_etat =
  let cos_theta = min (dot (neg uv) n) 1. in
  let r_out_perp = (uv +| (n */ cos_theta)) */ etai_over_etat in
  let r_out_parallel =
    n */ -.sqrt (abs_float 1. -. length_squared r_out_perp)
  in
  r_out_perp +| r_out_parallel


let reflect v n = v -| (n */ (2. *. dot v n))
