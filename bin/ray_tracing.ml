open Lib.Vec3
open Lib.Color

let image_width = 256

let image_height = 256

let render_point i j =
  let to_frac i total = Float.of_int i /. Float.of_int (total - 1) in
  let r = to_frac i image_width in
  let g = to_frac j image_height in
  let b = 0.25 in
  { x = r; y = g; z = b }

let render =
  print_string
    ("P3\n" ^ string_of_int image_width ^ " " ^ string_of_int image_height ^ " "
   ^ "\n255\n");
  for j = image_height - 1 downto 0 do
    Printf.eprintf "\rScanlines remaining: %d\n" j;
    for i = 0 to image_width - 1 do
      print_string (write_color (render_point i j))
    done
  done

let () = render
