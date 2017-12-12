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
  let models = Array.init nb (fun i -> Array.make size_vec 0.) in
  { models = models; pas = 0.05; pas_fact = 1e-1 }

let prod_scal w x = (* ok *)
  let length = Array.length w in
  let sum = ref 0. in
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
    let etiquette = if k = data.label then 1. else -1. in
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
  perc.pas <- perc.pas *. perc.pas_fact;
  !nb_err

let predict perc data =
  let nb_neuronne = Array.length perc.models in
  let a = Array.init nb_neuronne
      (fun k -> prod_scal perc.models.(k) data.vec)
  in
  arg_max a
