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


let rec range a b = if a >= b then [] else a :: range (a + 1) b

let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)


let random_sphere a b =
  let open Math in
  let open Int in
  let choose_mat = random_frac () in
  let center =
    Vec3.create
      (to_float a +. (0.9 *. random_frac ()))
      0.2
      (to_float b +. (0.9 *. random_frac ()))
  in
  if length (center -| Vec3.create 4. 0.2 0.) < 0.9
  then None
  else
    let sphere_material =
      if choose_mat < 0.8
      then
        (* diffuse *)
        let albedo = Vec3.random () *| Vec3.random () in
        Material.Lambertian { albedo }
      else if choose_mat < 0.95
      then
        (* metal *)
        let albedo = Vec3.random_range 0.5 1. in
        let fuzz = Math.random_range 0. 0.5 in
        Material.Metal { albedo; fuzz }
      else (* glass *)
        Material.Dielectric { refraction_index = 1.5 }
    in

    Some (Sphere.create center 0.2 sphere_material)


let random_scene () =
  Hittable.World
    (List.map
       (fun s -> Hittable.Sphere s)
       ( Sphere.create
           (Vec3.create 0. (-1000.) 0.)
           1000.
           (Material.Lambertian { albedo = Vec3.create 0.5 0.5 0.5 })
       :: Sphere.create
            (Vec3.create 0. 1. 0.)
            1.
            (Material.Dielectric { refraction_index = 1.5 })
       :: Sphere.create
            (Vec3.create (-4.) 1. 0.)
            1.
            (Material.Lambertian { albedo = Vec3.create 0.4 0.2 0.1 })
       :: Sphere.create
            (Vec3.create 4. 1. 0.)
            1.
            (Material.Metal { albedo = Vec3.create 0.7 0.6 0.5; fuzz = 0. })
       :: List.flatten
            (List.map
               (fun (a, b) ->
                 match random_sphere a b with Some s -> [ s ] | None -> [] )
               (cartesian (range (-11) 10) (range (-11) 10)) ) ) )


let render =
  let () = Random.self_init () in
  let samples_per_pixel = 500 in
  let aspect_ratio = 3. /. 2. in
  let image_width = 1200 in
  let image_height = Int.of_float (Float.of_int image_width /. aspect_ratio) in
  print_string
    ( "P3\n"
    ^ string_of_int image_width
    ^ " "
    ^ string_of_int image_height
    ^ " "
    ^ "\n255\n" ) ;

  let lookfrom = Vec3.create 13. 2. 3. in
  let lookat = Vec3.create 0. 0. 0. in
  let camera =
    Camera.create
      lookfrom
      lookat
      (Vec3.create 0. 1. 0.)
      20.
      aspect_ratio
      0.1
      10.
  in
  let world = random_scene () in
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
