type image = { vectorized_img : int array; label : int }

type set = image array

let load_img name label =
  let src = OImages.rgb24 (OImages.load name []) in
  let offset = src#width * src#height in
  let vec = Array.make (offset * 3) 0 in
  for x = 0 to src#width - 1 do
    for y = 0 to src#height - 1 do
      let rgb = src#get x y in
      vec.(x * src#width + y) <- rgb.r;
      vec.(offset + x * src#width + y) <- rgb.g;
      vec.(offset * 2 + x * src#width + y) <- rgb.b;
    done;
  done;
  {
    vectorized_img = vec;
    label = label
  }
