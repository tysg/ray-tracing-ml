let random_frac () = Random.float 1.0

let random_range min max = min +. (max -. min) *. random_frac()