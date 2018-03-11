let list_init f nb =
  let rec aux i f acc =
    if i = 0 then
      acc
    else
      aux (i - 1) f ((f i)::acc)
  in
  aux nb f []
