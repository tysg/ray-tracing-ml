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

