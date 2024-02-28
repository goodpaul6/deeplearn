type t = {
  weights : float array array;
  biases : float array;
  act_fn : float -> float;
}

let sigmoid v = 1.0 /. (1.0 +. exp v)
let create weights biases act_fn = { weights; biases; act_fn }

let prop values net =
  let rec loop values =
    let new_values = Mat.dot net.weights values in
    let new_values = Mat.add new_values net.biases in
    let new_values = Array.map net.act_fn new_values in
    if new_values = values then new_values else loop new_values
  in
  loop values
