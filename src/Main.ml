open OpenMnist
open MultiClassPerceptron

let usage = "usage: ./Main.native [options]"

let mnist = ref true
let nb_training_mnist_img = ref 6000

let spec =
  [
    "-mnist", Arg.Tuple [Arg.Set_int nb_training_mnist_img; Arg.Set mnist], "  on mnist training set"
  ]

let parse_args =
  Arg.parse spec (fun a -> ()) usage

let train_mnist () =
  Random.init (int_of_float (Unix.gettimeofday ()));
  let img_set, info = mk_train_set !nb_training_mnist_img in
  let norm_set = normalize_set_mnist img_set in
  let norm_set = randomize_order norm_set in
  print_info info;
  let perc = init_perceptron 10 (28 * 28) in
  for i = 0 to 20 do
    let nb_err = epoch perc norm_set in
    Printf.printf "Error(s) : %d, pas %f\n%!" nb_err perc.pas;
  done;
  perc

let test_mnist perc =
  let img_set, info = mk_test_set 10000 in
  let norm_set = normalize_set_mnist img_set in
  let norm_set = randomize_order norm_set in
  (* On test le modèle *)
  let separator = String.make 56 '=' in
  Printf.printf "%s\nTest perceptron (y) ? " separator;
  while read_line () = "y" do
    let random_index = Random.int (Array.length norm_set) in
    Printf.printf "What is this number ? :\n";
    print_data norm_set.(random_index);
    let test_img = norm_set.(random_index) in
    Printf.printf "Perceptron predict %d, real : %d\n" (predict perc test_img) test_img.label;
    Printf.printf "%s\nTest perceptron (y) ? " separator;
  done

let test_all_mnist perc =
  let img_set, info = mk_test_set 10000 in
  let norm_set = normalize_set_mnist img_set in
  let norm_set = randomize_order norm_set in
  let aux acc img =
    let prediction = predict perc img in
    if prediction = img.label then acc else acc + 1
  in
  let nb_err = Array.fold_left aux 0 norm_set in
  Printf.printf "Test the model on %d digits :\n" 10000;
  Printf.printf "Error(s) : %d\n" nb_err

let () =
  if !mnist then begin
    let perc = train_mnist () in
    test_mnist perc;
    test_all_mnist perc
  end;
