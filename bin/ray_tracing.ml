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


let rec ray_color hittable ray depth_limit =
  if depth_limit <= 0
  then Vec3.zero
  else
    match Hittable.hit ray 0.001 Float.max_float hittable with
    | Some hit_rec ->
      ( match Material.scatter hit_rec ray with
      | Some { attenuation; scattered_ray } ->
          attenuation *| ray_color hittable scattered_ray (depth_limit - 1)
      | None ->
          Color.black )
    | None ->
        let unit_direction = unit_vector ray.direction in
        let t = 0.5 *. (unit_direction.y +. 1.0) in
        gradient Color.white Color.light_blue t


let render =
  let () = Random.self_init () in
  let samples_per_pixel = 100 in
  let camera = Camera.create 90. (16. /. 9.) in
  let aspect_ratio = 16. /. 9. in
  let image_width = 400 in
  let image_height = Int.of_float (Float.of_int image_width /. aspect_ratio) in
  print_string
    ( "P3\n"
    ^ string_of_int image_width
    ^ " "
    ^ string_of_int image_height
    ^ " "
    ^ "\n255\n" ) ;

  let material_left = Material.Lambertian { albedo = Vec3.create 0. 0. 1. } in
  let material_right = Material.Lambertian { albedo = Vec3.create 1. 0. 0. } in

  let r = Float.cos (Float.pi /. 4.) in
  let world =
    Hittable.World
      [ Hittable.Sphere
          (Sphere.create (Vec3.create (-.r) 0. (-1.)) r material_left)
      ; Hittable.Sphere
          (Sphere.create (Vec3.create r 0. (-1.)) r material_right)
      ]
  in
  for j = image_height - 1 downto 0 do
    Printf.eprintf "Scanlines remaining: %d\n" j ;
    flush stderr ;
    for i = 0 to image_width - 1 do
      let to_frac i total =
        (Float.of_int i +. Math.random_frac ()) /. Float.of_int (total - 1)
      in
      let r = ref 0. in
      let g = ref 0. in
      let b = ref 0. in

      for _ = 0 to samples_per_pixel - 1 do
        let u = to_frac i image_width in
        let v = to_frac j image_height in
        let ray = Camera.get_ray camera u v in
        let color = ray_color world ray 50 in
        r := !r +. color.x ;
        g := !g +. color.y ;
        b := !b +. color.z
      done ;
      Color.write_color (Vec3.create !r !g !b) samples_per_pixel |> print_string
    done
  done
