open Vec3

type viewport =
  { aspect_ratio : float
  ; height : float
  ; width : float
  ; focal_length : float
  }

let viewport =
  let aspect_ratio = 16. /. 9. in
  let height = 2. in
  let width = aspect_ratio *. height in
  let focal_length = 1.0 in
  { aspect_ratio; height; width; focal_length }


type t =
  { origin : Vec3.t
  ; lower_left_corner : Vec3.t
  ; horizontal : Vec3.t
  ; vertical : Vec3.t
  }

let create viewport =
  let origin = Vec3.zero in
  let horizontal = Vec3.create viewport.width 0. 0. in
  let vertical = Vec3.create 0. viewport.height 0. in
  let lower_left_corner =
    origin
    -| (horizontal // 2.)
    -| (vertical // 2.)
    -| Vec3.create 0. 0. viewport.focal_length
  in
  { origin; lower_left_corner; horizontal; vertical }


let get_ray (camera : t) u v : Ray.t =
  let direction =
    camera.lower_left_corner
    +| (camera.horizontal */ u)
    +| (camera.vertical */ v)
    -| camera.origin
  in
  Ray.create camera.origin direction
