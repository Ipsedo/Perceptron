open Utils

type perceptron =
  {
    mutable models : float list list;
    mutable pas : float;
    pas_fact : float
  }

type normalized_data =
  {
    vec : float list;
    label : int
  }

type data_set = normalized_data array

let randomize_order set =
  let nd = List.rev_map (fun c -> (Random.bits (), c)) set in
  let nd = List.sort compare nd in
  List.rev_map snd nd

let init_perceptron nb size_vec =
  let init_neur () = list_init (fun _ -> 0.5) size_vec in
  { models = list_init (fun _ -> init_neur ()) nb;
    pas = 0.05;
    pas_fact = 0.8 }

let prod_scal w x =
  List.fold_left2 (fun acc wi xi -> acc +. wi *. xi) 0.0 w x

let arg_max a =
  let is_max (m, i, curr) elt =
    if elt > m then (elt, curr, curr + 1)
    else (m, i, curr + 1)
  in
  let (_, res, _) = List.fold_left is_max (-. max_float, -1, 0) a in
  res

let th_vec a =
  List.rev_map tanh a

let update_perceptron perc g data =
  let compute_one pas k w_k g_k data =
    let etiquette = if k = data.label then 1. else -1. in
    List.map2 (fun w_k_j data_j -> w_k_j -. pas *. (g_k -. etiquette) *. data_j) w_k data.vec
  in

  let (res, _) = List.fold_right2
      (fun w_k g_k (acc, k) -> ((compute_one perc.pas k w_k g_k data)::acc, k - 1))
      perc.models g ([], 9)
  in
  res

let epoch perc data_set =
  let aux acc img =
    let a = List.rev_map
        (fun model_k -> prod_scal model_k img.vec)
        perc.models
    in
    if img.label <> arg_max (List.rev a) then begin
      perc.models <- update_perceptron perc (th_vec a) img;
      acc + 1
    end
    else
      acc
  in
  let nb_err = List.fold_left aux 0 data_set in
  perc.pas <- perc.pas *. perc.pas_fact;
  nb_err

let predict perc data =
  let a = List.rev_map
      (fun model_k -> prod_scal model_k data.vec)
      perc.models
  in
  arg_max (List.rev a)
