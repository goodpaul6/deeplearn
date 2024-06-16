let mean_squared_loss net inputs expected_outputs =
  let n = Array.length inputs in
  assert (n = Array.length expected_outputs);
  let sum = ref 0.0 in
  for i = 0 to Array.length inputs do
    let output = Neural_net.prop inputs.(i) net in
    let expected_output = expected_outputs.(i) in
    let sq_loss = Mat.sub expected_output output |> Mat.len2 in
    sum := !sum +. sq_loss
  done;
  0.5 *. float_of_int n *. !sum
