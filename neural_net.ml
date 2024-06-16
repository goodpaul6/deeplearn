open Mat

(* every row `i` of weights corresponds to node `i` and every column `j`
   refers to an edge incident on that node. Hence, the value at `(i, j)`
   is the weight of said edge. *)
type t = { weights : matrix; biases : vector; act_fn : float -> float }

let sigmoid v = 1.0 /. (1.0 +. exp v)
let create weights biases act_fn = { weights; biases; act_fn }

let prop values net =
  let rec loop values =
    let new_values = mul_mat_vec net.weights values in
    let new_values = add_vec_vec new_values net.biases in
    let new_values = Array.map net.act_fn new_values in
    if new_values = values then new_values else loop new_values
  in
  loop values
