type t =
  { x : float
  ; y : float
  ; z : float
  }

let create x y z = { x; y; z }

let length_squared v = (v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)

let length v = sqrt (length_squared v)

let to_string v =
  string_of_float v.x ^ " " ^ string_of_float v.y ^ " " ^ string_of_float v.z


let ( +| ) u v = { x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z }

let ( +/ ) u n = { x = u.x +. n; y = u.y +. n; z = u.z +. n }

let ( -| ) u v = { x = u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z }

let ( -/ ) u v = { x = u.x -. v; y = u.y -. v; z = u.z -. v }

(** vector multiplication *)
let ( *| ) u v = { x = u.x *. v.x; y = u.y *. v.y; z = u.z *. v.z }

(** scalar division *)
let ( // ) u t = { x = u.x /. t; y = u.y /. t; z = u.z /. t }

(** scalar multiplication *)
let ( */ ) u n = { x = u.x *. n; y = u.y *. n; z = u.z *. n }

let dot u v = (u.x *. v.x) +. (u.y *. v.y) +. (u.z *. v.z)

let cross u v =
  { x = (u.y *. v.z) -. (u.z *. v.y)
  ; y = (u.z *. v.x) -. (u.x *. v.z)
  ; z = (u.x *. v.y) -. (u.y *. v.x)
  }


let unit_vector v = v // length v

let zero = { x = 0.; y = 0.; z = 0. }

let neg v = create (-.v.x) (-.v.y) (-.v.z)

let map_vec3 v fn = { x = fn v.x; y = fn v.y; z = fn v.z }

let random () =
  create (Math.random_frac ()) (Math.random_frac ()) (Math.random_frac ())


let random_range min max =
  create
    (Math.random_range min max)
    (Math.random_range min max)
    (Math.random_range min max)


let rec random_in_unit_sphere () =
  let p = random_range (-1.) 1. in
  if length_squared p < 1. then p else random_in_unit_sphere ()


let random_unit_vector () = unit_vector (random_in_unit_sphere ())

let random_in_hemisphere normal =
  let in_unit_sphere = random_in_unit_sphere () in
  if dot in_unit_sphere normal > 0. then in_unit_sphere else neg in_unit_sphere


let is_near_zero u =
  let s = 1e-8 in
  u.x < s && u.y < s && u.z < s


let rec random_in_unit_disk () =
  let p = create (Random.float 2. -. 1.) (Random.float 2. -. 1.) 0. in
  if length_squared p >= 1. then random_in_unit_disk () else p
