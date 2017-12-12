type perceptron = { models : float array array; mutable pas : float }

type normalized_data = { vec : float array; label : int }

type data_set = normalized_data array

let normalize_img_mnist img = (* ok *)
  { vec = Array.map (fun a -> (float_of_int a) /. 255.0) img.OpenMnist.pixels; label = img.label }

let print_data img = (* ok *)
  (*for i = 0 to (Array.length img.vec - 1) do
    let to_print = if img.vec.(i) > 0.5 then "#" else "." in
    let to_print = to_print ^ (if i mod 28 = 0 && i <> 0 then "\n" else "") in
    Printf.printf "%s " to_print;
    done;*)
  for i = 0 to 27 do
    for j = 0 to 27 do
      let to_print = if img.vec.(i * 28 + j) > 0.5 then "#" else "." in
      Printf.printf "%s " to_print;
    done;
    Printf.printf "\n"
  done

let normalize_set_mnist set = (* ok *)
  Array.map normalize_img_mnist set

let randomize_order set = (* ok *)
  let nd = Array.map (fun c -> (Random.bits (), c)) set in
  Array.sort compare nd;
  Array.map snd nd

let init_perceptron nb size_vec = (* ok *)
  let models = Array.init nb (fun i -> Array.make size_vec 0.0) in
  { models = models; pas = 0.05 }

let prod_scal w x = (* ok *)
  let length = Array.length w in
  let sum = ref 0.0 in
  for i = 0 to length - 1 do
    sum := !sum +. w.(i) *. x.(i);
  done;
  !sum

let arg_max a = (* ok *)
  let length = Array.length a in
  let a_max = ref 0 in
  for i = 0 to length - 1 do
    if a.(!a_max) < a.(i) then a_max := i;
  done;
  !a_max

let th_vec a = (* ok *)
  let length = Array.length a in
  for i = 0 to length - 1 do
    a.(i) <- tanh a.(i);
  done

let update_perceptron perc g data =
  let nb_neuronne = Array.length perc.models in
  let size_vec = Array.length data.vec in
  for k = 0 to nb_neuronne - 1 do
    let etiquette = if k = data.label then 1.0 else -1.0 in
    for j = 0 to size_vec - 1 do
      perc.models.(k).(j) <- perc.models.(k).(j) -. perc.pas *. (g.(k) -. etiquette) *. data.vec.(j);
    done;
  done

let epoch perc data_set =
  let nb_err = ref 0 in
  let size = Array.length data_set in
  for i = 0 to size - 1 do
    let nb_neuronne = Array.length perc.models in
    let a = Array.init nb_neuronne
        (fun k -> prod_scal perc.models.(k) data_set.(i).vec)
    in
    if data_set.(i).label <> arg_max a then begin
      incr nb_err;
      th_vec a;
      update_perceptron perc a data_set.(i);
    end;
  done;
  perc.pas <- perc.pas /. 1.1;
  !nb_err

let predict perc data =
  let nb_neuronne = Array.length perc.models in
  let a = Array.init nb_neuronne
      (fun k -> prod_scal perc.models.(k) data.vec)
  in
  arg_max a
