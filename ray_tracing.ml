let render = 
  let image_width = 256 in
  let image_height = 256 in 
 let render_point i j = 
    let r = Float.of_int i /. Float.of_int (image_width - 1) in
    let g = Float.of_int j /. Float.of_int (image_height - 1) in 
    let b = 0.25 in
    Array.map (fun n -> Int.of_float (255.999 *. n)) [|r;g;b|]
  in 
  print_endline ("P3\n" ^ string_of_int image_width ^ " " ^ string_of_int image_height ^ " " ^ "\n255\n");
  for j = image_height-1 downto 0 do
    Printf.eprintf "\rScanlines remaining: %d\n" j;
    for i = 0 to image_width -1 do
      let p = render_point i j in
      print_endline (string_of_int (p.(0)) ^ " " ^ string_of_int (p. (1)) ^ " "^ string_of_int(p.(2)));
    done
  done

let () = render

