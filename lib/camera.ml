open Vec3

type t =
  { origin : Vec3.t
  ; lower_left_corner : Vec3.t
  ; horizontal : Vec3.t
  ; vertical : Vec3.t
  ; lens_radius : float
  }

type viewport =
  { aspect_ratio : float
  ; height : float
  ; width : float
  ; focal_length : float
  }

let create lookfrom lookat vup vfov aspect_ratio aperture focus_dist =
  let viewport_height = 2. *. Float.tan (Math.to_radians vfov /. 2.) in
  let viewport_width = aspect_ratio *. viewport_height in

  let w = unit_vector (lookfrom -| lookat) in
  let u = unit_vector (cross vup w) in
  let v = cross w u in

  let origin = lookfrom in
  let horizontal = u */ (viewport_width *. focus_dist) in
  let vertical = v */ (viewport_height *. focus_dist) in

  let lower_left_corner =
    origin -| (horizontal // 2.) -| (vertical // 2.) -| (w */ focus_dist)
  in
  { origin
  ; lower_left_corner
  ; horizontal
  ; vertical
  ; lens_radius = aperture /. 2.
  }


let get_ray (camera : t) u v : Ray.t =
  let rd = random_in_unit_disk () */ camera.lens_radius in
  let offset = (rd.x *. u) +. (rd.y *. v) in
  let direction =
    camera.lower_left_corner
    +| (camera.horizontal */ u)
    +| (camera.vertical */ v)
    -| camera.origin
    -/ offset
  in
  Ray.create (camera.origin +/ offset) direction
