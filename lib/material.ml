open Vec3

type t =
  | Lambertian of { albedo : Vec3.t }
  (* fuzz < 1 *)
  | Metal of
      { albedo : Vec3.t
      ; fuzz : float
      }
  | Dielectric of { refraction_index : float }

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
        Ray.reflect (unit_vector ray_in.direction) hit_rec.normal
        +| (random_in_unit_sphere () */ fuzz)
      in
      Base.Option.some_if
        (dot reflected hit_rec.normal > 0.)
        { scattered_ray = Ray.create hit_rec.point reflected
        ; attenuation = albedo
        }
  | Dielectric { refraction_index = ir } ->
      let refraction_ratio =
        match hit_rec.facing with Front -> 1. /. ir | Back -> ir
      in
      let unit_direction = unit_vector ray_in.direction in
      let cos_theta = Float.min 1.0 (dot (neg unit_direction) hit_rec.normal) in
      let sin_theta = sqrt 1. -. (cos_theta *. cos_theta) in
      let reflectance cos ref_idx =
        let r0 = (1. -. ref_idx) /. (1. +. ref_idx) in
        let r0 = r0 *. r0 in
        r0 +. ((1. -. r0) *. Float.pow (1. -. cos) 5.)
      in
      let direction =
        if refraction_ratio *. sin_theta > 1.
           || reflectance cos_theta refraction_ratio > Math.random_frac ()
        then Ray.reflect unit_direction hit_rec.normal
        else Ray.refract unit_direction hit_rec.normal refraction_ratio
      in
      Some
        { scattered_ray = Ray.create hit_rec.point direction
        ; attenuation = Color.white
        }
