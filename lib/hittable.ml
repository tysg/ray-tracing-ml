open Material
type t =
  | Sphere of Sphere.t
  | World of t list

let rec hit (ray : Ray.t) (t_min : float) (t_max : float) (htb : t) :
    Material.hit_record option =
  match htb with
  | Sphere s ->
      Sphere.hit ray t_min t_max s
  | World world ->
      let f acc obj =
        match acc with
        | None ->
            hit ray t_min t_max obj
        | Some hit_rec as opt ->
            let new_rec = hit ray t_min hit_rec.t obj in
            if Option.is_some new_rec then new_rec else opt
      in
      List.fold_left f None world
