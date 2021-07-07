open Vec3

type t = { origin : Vec3.t; direction : Vec3.t }

type hit_record = {point: Vec3.t; normal: Vec3.t; t: float}

let create origin direction = { origin; direction }

let at r t = r.origin +| (r.direction */ t)

let ( @/ ) r t = at r t
