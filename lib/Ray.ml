open Vec3

type ray = { origin : point3; direction : vec3 }

let at r t = r.origin +| (r.direction */ t)
