open Vec3

type t =
  | Lambertian of { albedo : Vec3.t }
  (* fuzz < 1 *)
  | Metal of
      { albedo : Vec3.t
      ; fuzz : float
      }

type hit_record =
  { point : Vec3.t
  ; normal : Vec3.t
  ; t : float
  ; facing : Ray.facing_direction
  ; material : t
  }

type scatter_result =
  { attenuation : Vec3.t
  ; scattered_ray : Ray.t
  }

let scatter (hit_rec : hit_record) (ray_in : Ray.t) : scatter_result option =
  match hit_rec.material with
  | Lambertian { albedo } ->
      let scatter_direction = hit_rec.normal +| Vec3.random_unit_vector () in
      let scatter_direction =
        if is_near_zero scatter_direction
        then hit_rec.normal
        else scatter_direction
      in
      Some
        { scattered_ray = Ray.create hit_rec.point scatter_direction
        ; attenuation = albedo
        }
  | Metal { albedo; fuzz } ->
      let fuzz = if fuzz < 1. then fuzz else 1. in
      let reflected =
        reflect (unit_vector ray_in.direction) hit_rec.normal
        +| (random_in_unit_sphere () */ fuzz)
      in
      Base.Option.some_if
        (dot reflected hit_rec.normal > 0.)
        { scattered_ray = Ray.create hit_rec.point reflected
        ; attenuation = albedo
        }
