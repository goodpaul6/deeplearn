module La = Linalg

type t = {
  (* We use an array of matrices because we have multiple layers *)
  mutable weights : La.matrix array;
  mutable biases : La.vector array;
  learning_rate : float;
  act_fn : float -> float;
}

let sigmoid x = 1.0 /. (1.0 +. exp (-.x))
let d_sigmoid_given_sigmoid sx = sx *. (1.0 -. sx)

let create input_nodes hidden_nodes output_nodes =
  let rand () = Random.float 2.0 -. 1.0 in
  let weights =
    [|
      (* Rows = nodes, columns = edges going into the node *)

      (* Weights from input to hidden *)
      La.mat_init hidden_nodes input_nodes (fun _ _ -> rand ());
      (* Weights from hidden to output *)
      La.mat_init output_nodes hidden_nodes (fun _ _ -> rand ());
    |]
  in
  let biases =
    [|
      (* One bias per node in each layer *)
      Array.init hidden_nodes (fun _ -> rand ());
      Array.init output_nodes (fun _ -> rand ());
    |]
  in
  { weights; biases; learning_rate = 0.05; act_fn = sigmoid }

let feedforward_retain_outputs t inputs =
  (* TODO(Apaar): This can be made to work in-place with the array by modifying the Linalg funcs. Will just need the
     array to be long enough to accommodate every layer. *)
  let values = ref inputs in
  let outputs_list = ref [] in
  for i = 0 to Array.length t.weights - 1 do
    let layer_output = La.mul_mat_vec t.weights.(i) !values in
    let layer_output = La.add_vec_vec layer_output t.biases.(i) in
    let layer_output = Array.map t.act_fn layer_output in
    values := layer_output;
    outputs_list := !values :: !outputs_list
  done;
  !outputs_list |> List.rev |> Array.of_list

let last a = a.(Array.length a - 1)

let feedforward t inputs =
  let all_outputs = feedforward_retain_outputs t inputs in
  last all_outputs

let train t inputs targets =
  let all_outputs = feedforward_retain_outputs t inputs in
  let errors = ref @@ La.sub_vec_vec targets (last all_outputs) in
  for i = Array.length t.weights - 1 downto 0 do
    let layer_outputs = all_outputs.(i) in
    let prev_layer_outputs = if i > 0 then all_outputs.(i - 1) else inputs in
    let lr_errors_gradient =
      let lr_errors = La.mul_vec_scalar !errors t.learning_rate in
      let gradient = Array.map d_sigmoid_given_sigmoid layer_outputs in
      La.mul_vec_vec lr_errors gradient
    in
    (* We have to compute the weighted error from each output node. Since each row r in the weights
       corresponds to an output, and each column c to a hidden node,
       the error(c) = sum_over_every_r ( w_rc / sum_of_row(r) * error_r ).

       However, if you ignore the sum_of_row component (which we can since
       the result is still proportional), then this is just a multiplication
       between the TRANSPOSE of the weights matrix and the error vector *)
    let weights_transposed = La.mat_transpose t.weights.(i) in
    let prop_errors = La.mul_mat_vec weights_transposed !errors in
    errors := prop_errors;
    let delta_weights =
      La.mul_mat_mat
        (La.vec_to_col_mat lr_errors_gradient)
        (La.vec_to_row_mat prev_layer_outputs)
    in
    let delta_biases = lr_errors_gradient in
    t.weights.(i) <- La.add_mat_mat t.weights.(i) delta_weights;
    t.biases.(i) <- La.add_vec_vec t.biases.(i) delta_biases
  done
