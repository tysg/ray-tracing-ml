open Array

type vec3 = { x : float; y : float; z : float }

let length_squared v = (v.x ** 2.) +. (v.y ** 2.) +. (v.z ** 2.)

let length v = sqrt (length_squared v)

let to_string v =
  string_of_float v.x ^ " " ^ string_of_float v.y ^ " " ^ string_of_float v.z

let ( +| ) u v = { x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z }

let ( -| ) u v = { x = u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z }

let ( *| ) u v = { x = u.x *. v.x; y = u.y *. v.y; z = u.z *. v.z }

let ( /| ) (x, y, z) t = (x /. t, y /. t, z /. t)

let scale v n = (get_x v *. n, get_y v *. n, get_z v *. n)

let dot u v =
  (get_x u *. get_x v) +. (get_y u *. get_y v) +. (get_z u *. get_z v)

let cross (ux, uy, uz) (vx, vy, vz) =
  ((uy *. vz) -. (uz *. vy), (uz *. vx) -. (ux *. vz), (ux *. vy) -. (uy *. vx))

let unit_vector v = v /| length v
