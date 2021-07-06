open Lib.Vec3
open Lib.Color
open Lib.Ray

let gradient (x : color) (y : color) t = (x */ (1.0 -. t)) +| (y */ t)

let ray_color ray =
  let unit_direction = unit_vector ray.direction in
  let t = 0.5 *. (unit_direction.y +. 1.0) in
  let white = { x = 1.0; y = 1.0; z = 1.0 } in
  let light_blue = { x = 0.5; y = 0.7; z = 1.0 } in
  gradient white light_blue t

(* image *)
let aspect_ratio = 16. /. 9.

(* camera *)
let viewport_height = 2.0

let viewport_width = aspect_ratio *. viewport_height

let focal_length = 1.0

let origin = { x = 0.; y = 0.; z = 0. }

let horizontal = { x = viewport_width; y = 0.; z = 0. }

let vertical = { x = 0.; y = viewport_height; z = 0. }

let lower_left_corner =
  origin -| (horizontal // 2.) -| (vertical // 2.)
  -| { x = 0.; y = 0.; z = focal_length }

let render_ray u v =
  {
    origin;
    direction =
      lower_left_corner +| (horizontal */ u) +| (vertical */ v) -| origin;
  }

let render =
  let image_width = 400 in
  let image_height = Int.of_float (Float.of_int image_width /. aspect_ratio) in
  print_string
    ("P3\n" ^ string_of_int image_width ^ " " ^ string_of_int image_height ^ " "
   ^ "\n255\n");
  for j = image_height - 1 downto 0 do
    Printf.eprintf "\rScanlines remaining: %d\n" j;
    for i = 0 to image_width - 1 do
      let to_frac i total = Float.of_int i /. Float.of_int (total - 1) in
      let u = to_frac i image_width in
      let v = to_frac j image_height in
      render_ray u v |> ray_color |> write_color |> print_string
    done
  done

