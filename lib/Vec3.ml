type vec3 = { x : float; y : float; z : float }

let length_squared v = (v.x ** 2.) +. (v.y ** 2.) +. (v.z ** 2.)

let length v = sqrt (length_squared v)

let to_string v =
  string_of_float v.x ^ " " ^ string_of_float v.y ^ " " ^ string_of_float v.z

let ( +| ) u v = { x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z }

let ( -| ) u v = { x = u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z }

(** vector multiplication *)
let ( *| ) u v = { x = u.x *. v.x; y = u.y *. v.y; z = u.z *. v.z }

(** scalar division *)
let ( // ) u t = { x = u.x /. t; y = u.y /. t; z = u.z /. t }

(** scalar multiplication *)
let ( */ ) u n = { x = u.x *. n; y = u.y *. n; z = u.z *. n }

let dot u v = (u.x *. v.x) +. (u.y *. v.y) +. (u.z *. v.z)

let cross u v =
  {
    x = (u.y *. v.z) -. (u.z *. v.y);
    y = (u.z *. v.x) -. (u.x *. v.z);
    z = (u.x *. v.y) -. (u.y *. v.x);
  }

let unit_vector v = v // length v

type point3 = vec3

type color = vec3