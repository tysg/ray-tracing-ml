open Vec3
open Ray

type t = { center : Vec3.t; radius : float }

let find_root a half_b c (min : float) (max : float) =
  let delta = (half_b *. half_b) -. (a *. c) in
  if delta < 0. then None
  else
    let sqrtd = Float.sqrt delta in
    let try_find (root : float) =
      if root < min || root > max then None else Some root
    in
    match try_find ((-.half_b -. sqrtd) /. a) with
    | None -> try_find ((-.half_b +. sqrtd) /. a)
    | Some _ as opt -> opt

let hit (r : Ray.t) t_min t_max (sphere : t) : hit_record option =
  let oc = r.origin -| sphere.center in
  let a = length_squared r.direction in
  let half_b = dot oc r.direction in
  let c = length_squared oc -. (sphere.radius ** 2.) in
  match find_root a half_b c t_min t_max with
  | None -> None
  | Some root ->
      let p = r @/ root in
      let normal = (p -| sphere.center) // sphere.radius in
      Some { point = p; normal; t = root }
