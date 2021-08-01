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

let create lookfrom lookat vup vfov aspect_ratio =
  let viewport_height = 2. *. Float.tan (Math.to_radians vfov /. 2.) in
  let viewport_width = aspect_ratio *. viewport_height in

  let w = unit_vector (lookfrom -| lookat) in
  let u = unit_vector (cross vup w) in
  let v = cross w u in

  let origin = lookfrom in
  let horizontal = u */ viewport_width in
  let vertical = v */ viewport_height in

  let lower_left_corner =
    origin -| (horizontal // 2.) -| (vertical // 2.) -| w
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
