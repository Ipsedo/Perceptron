type perceptron =
  {
    models : float array array;
    mutable pas : float;
    pas_fact : float
  }

type normalized_data =
  {
    vec : float array;
    label : int
  }

type data_set = normalized_data array

let randomize_order set = (* ok *)
  let nd = Array.map (fun c -> (Random.bits (), c)) set in
  Array.sort compare nd;
  Array.map snd nd

let init_perceptron nb size_vec = (* ok *)
  { models = Array.make_matrix nb size_vec 0.;
    pas = 0.05;
    pas_fact = 0.8 }

let prod_scal w x = (* ok *)
  let length = Array.length w in
  let sum = ref 0. in
  for i = 0 to length - 1 do
    sum := !sum +. w.(i) *. x.(i);
  done;
  !sum

let arg_max a = (* ok *)
  let aux (acc, i) elt =
    if a.(acc) < a.(i) then (i, i + 1) else (acc, i + 1)
  in
  let (res,_ ) = Array.fold_left aux (0, 0) a in
  res

let th_vec a = (* ok *)
  Array.map tanh a

let update_perceptron perc g data =
  let nb_neuronne = Array.length perc.models in
  let size_vec = Array.length data.vec in
  for k = 0 to nb_neuronne - 1 do
    let etiquette = if k = data.label then 1. else -1. in
    for j = 0 to size_vec - 1 do
      perc.models.(k).(j) <- perc.models.(k).(j) -. perc.pas *. (g.(k) -. etiquette) *. data.vec.(j);
    done;
  done

let epoch perc data_set =
  let aux acc img =
    let nb_neuronne = Array.length perc.models in
    let a = Array.init nb_neuronne
        (fun k -> prod_scal perc.models.(k) img.vec)
    in
    update_perceptron perc (th_vec a) img;
    if img.label <> arg_max a then acc + 1 else acc
  in
  let nb_err = Array.fold_left aux 0 data_set in
  perc.pas <- perc.pas *. perc.pas_fact;
  nb_err

let predict perc data =
  let nb_neuronne = Array.length perc.models in
  let a = Array.init nb_neuronne
      (fun k -> prod_scal perc.models.(k) data.vec)
  in
  arg_max a
