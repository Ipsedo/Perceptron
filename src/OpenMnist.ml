type image = { pixels : int array; label : int }

type set_info =
  {
    magic_number_img : int;
    magic_number_label : int;
    number_of_img : int;
    number_of_label : int;
    img_resolution : int * int;

  }

type set = image array * set_info

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

let print_info info =
  Printf.printf "Image file\n";
  Printf.printf "Magic number : %d\n" info.magic_number_img;
  Printf.printf "Number of image : %d\n" info.number_of_img;
  Printf.printf "Images are %d * %d\n" (fst info.img_resolution) (snd info.img_resolution);
  Printf.printf "Label file:\n";
  Printf.printf "Magic number : %d\n" info.magic_number_label;
  Printf.printf "Number of label : %d%!\n" info.number_of_label

let mk_set nb_img images_file labels_file =
  let ic_train = open_in (res_folder ^ images_file) in
  let magic_number_img = input_binary_int ic_train in
  let number_of_img = input_binary_int ic_train in
  let img_resolution = (input_binary_int ic_train), (input_binary_int ic_train) in
  let ic_label = open_in (res_folder ^ labels_file) in
  let magic_number_label = input_binary_int ic_label in
  let number_of_label = input_binary_int ic_label in
  let train_set = mk_train_set ic_train ic_label nb_img in
  close_in ic_train;
  close_in ic_label;
  train_set,
  {
    magic_number_img = magic_number_img;
    number_of_img = number_of_img;
    img_resolution = img_resolution;
    magic_number_label = magic_number_label;
    number_of_label = number_of_label
  }

let mk_train_set nb_img =
  mk_set nb_img "train-images.idx3-ubyte" "train-labels.idx1-ubyte"

let mk_test_set nb_img =
  mk_set nb_img "t10k-images.idx3-ubyte" "t10k-labels.idx1-ubyte"

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
