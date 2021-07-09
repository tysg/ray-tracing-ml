open Lib
open Lib.Vec3
open Lib.Ray

let gradient (x : Vec3.t) (y : Vec3.t) t = (x */ (1.0 -. t)) +| (y */ t)

let hit_sphere (center : Vec3.t) (radius : float) (ray : Ray.t) : float option =
  let oc = ray.origin -| center in
  let a = length_squared ray.direction in
  let half_b = dot oc ray.direction in
  let c = length_squared oc -. (radius *. radius) in
  let delta = (half_b *. half_b) -. (a *. c) in
  if delta >= 0. then Some ((~-.half_b -. sqrt delta) /. a) else None


let hit_world world ray =
  match Sphere.hit_list ray 0. Float.max_float world with
  | Some hit_rec ->
      (hit_rec.normal +/ 1.) */ 0.5
  | None ->
      let unit_direction = unit_vector ray.direction in
      let t = 0.5 *. (unit_direction.y +. 1.0) in
      gradient Color.white Color.light_blue t


(* image *)
let aspect_ratio = 16. /. 9.

(* camera *)
let viewport_height = 2.0

let viewport_width = aspect_ratio *. viewport_height

let focal_length = 1.0

let origin = Vec3.zero

let horizontal = Vec3.create viewport_width 0. 0.

let vertical = Vec3.create 0. viewport_height 0.

let lower_left_corner =
  origin
  -| (horizontal // 2.)
  -| (vertical // 2.)
  -| Vec3.create 0. 0. focal_length


let render_ray u v =
  Ray.create
    origin
    (lower_left_corner +| (horizontal */ u) +| (vertical */ v) -| origin)


let render =
  let image_width = 400 in
  let image_height = Int.of_float (Float.of_int image_width /. aspect_ratio) in
  print_string
    ( "P3\n"
    ^ string_of_int image_width
    ^ " "
    ^ string_of_int image_height
    ^ " "
    ^ "\n255\n" ) ;
  let world =
    [ 
     Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100.;
      Sphere.create (Vec3.create 0. 0. (-1.)) 0.5;
    ]
  in
  for j = image_height - 1 downto 0 do
    Printf.eprintf "\rScanlines remaining: %d\n" j ;
    for i = 0 to image_width - 1 do
      let to_frac i total = Float.of_int i /. Float.of_int (total - 1) in
      let u = to_frac i image_width in
      let v = to_frac j image_height in
      render_ray u v |> hit_world world |> Color.write_color |> print_string
    done
  done
