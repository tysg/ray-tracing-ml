open Vec3

type t =
  { origin : Vec3.t
  ; lower_left_corner : Vec3.t
  ; horizontal : Vec3.t
  ; vertical : Vec3.t
  }

type viewport =
  { aspect_ratio : float
  ; height : float
  ; width : float
  ; focal_length : float
  }

let create vfov aspect_ratio =
  let viewport_height = 2. *. Float.tan (Math.to_radians vfov /. 2.) in
  let viewport_width = aspect_ratio *. viewport_height in
  let focal_length = 1.0 in

  let origin = Vec3.zero in
  let horizontal = Vec3.create viewport_width 0. 0. in
  let vertical = Vec3.create 0. viewport_height 0. in
  let lower_left_corner =
    origin
    -| (horizontal // 2.)
    -| (vertical // 2.)
    -| Vec3.create 0. 0. focal_length
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
