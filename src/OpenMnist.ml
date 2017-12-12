type image = { pixels : int array; label : int }

type set = image array

let res_folder = "./res/"

let mk_image ic_train ic_label =
  let nb_pixel = 28 * 28 in
  let pixels = Array.make nb_pixel 0 in
  for i=0 to (nb_pixel - 1) do
    pixels.(i) <- input_byte ic_train;
  done;
  let label = input_byte ic_label in
  { pixels = pixels; label = label }

let print_image img =
  for i = 0 to (Array.length img.pixels - 1) do
    let to_print = if img.pixels.(i) > 200 then "#" else "." in
    let to_print = to_print ^ (if i mod 28 = 0 && i <> 0 then "\n" else "") in
    Printf.printf "%s " to_print;
  done;
  Printf.printf "\n"

let mk_train_set ic_train ic_label nb_img =
  let set = Array.make nb_img { pixels = Array.make 0 0; label = -1 } in
  for i = 0 to (nb_img - 1) do
    set.(i) <- mk_image ic_train ic_label;
  done;
  set

let mk_train_test nb_img =
  let ic_train = open_in (res_folder ^ "train-images.idx3-ubyte") in
  Printf.printf "Train file\n";
  Printf.printf "Magic number : %d\n" (input_binary_int ic_train);
  Printf.printf "Number of image : %d\n" (input_binary_int ic_train);
  Printf.printf "Images are %d * %d\n" (input_binary_int ic_train) (input_binary_int ic_train);

  let ic_label = open_in (res_folder ^ "train-labels.idx1-ubyte") in
  Printf.printf "Label file:\n";
  Printf.printf "Magic number : %d\n" (input_binary_int ic_label);
  Printf.printf "Number of label : %d\n" (input_binary_int ic_label);
  let train_set = mk_train_set ic_train ic_label nb_img in
  close_in ic_train;
  close_in ic_label;
  train_set

open MultiClassPerceptron

let normalize_img_mnist img = (* ok *)
  {
    vec = Array.map (fun a -> (float_of_int a) /. 255.) img.pixels;
    label = img.label
  }

let print_data img = (* ok *)
  for i = 0 to 27 do
    for j = 0 to 27 do
      let to_print = if img.vec.(i * 28 + j) > 0.5 then "#" else "." in
      Printf.printf "%s " to_print;
    done;
    Printf.printf "\n"
  done

let normalize_set_mnist set = (* ok *)
  Array.map normalize_img_mnist set
