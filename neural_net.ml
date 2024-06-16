module La = Linalg

type t = { weights : La.matrix; biases : La.vector; act_fn : float -> float }

let sigmoid v = 1.0 /. (1.0 +. exp v)
let create weights biases act_fn = { weights; biases; act_fn }

let prop values net =
  let rec loop values =
    let new_values = La.mul_mat_vec net.weights values in
    let new_values = La.add_vec_vec new_values net.biases in
    let new_values = Array.map net.act_fn new_values in
    if new_values = values then new_values else loop new_values
  in
  loop values
