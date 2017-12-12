open OpenMnist
open MultiClassPerceptron

let () =
  let img_set = mk_train_test 6000 in
  let norm_set = normalize_set_mnist img_set in
  let norm_set = randomize_order norm_set in
  let perc = init_perceptron 10 (28 * 28) in
  for i = 0 to 10 do
    let nb_err = epoch perc norm_set in
    Printf.printf "Error(s) : %d, pas %f\n" nb_err perc.pas;
  done;
  Printf.printf "Test perceptron (y / n) ? ";
  let will_test = ref (read_line () = "y") in
  Printf.printf "\n";
  while !will_test do
    let random_index = Random.int (Array.length norm_set) in
    Printf.printf "What is this number ? :\n";
    print_data norm_set.(random_index);
    Printf.printf "Perceptron predict %d\n" (predict perc norm_set.(random_index));
    Printf.printf "Test perceptron (y / n) ? ";
    will_test := read_line () = "y";
    Printf.printf "\n";
  done
