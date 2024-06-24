module La = Linalg

type t = {
  (* We use an array of matrices because we have multiple layers *)
  weights : La.matrix array;
  biases : La.vector array;
  act_fn : float -> float;
}

let sigmoid x = 1.0 /. (1.0 +. exp (-.x))

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
  { weights; biases; act_fn = sigmoid }

let feedforward t inputs =
  (* TODO(Apaar): This can be made to work in-place with the array by modifying the Linalg funcs. Will just need the
     array to be long enough to accommodate every layer. *)
  let values = ref inputs in
  for i = 0 to Array.length t.weights - 1 do
    let layer_output = La.mul_mat_vec t.weights.(i) !values in
    let layer_output = La.add_vec_vec layer_output t.biases.(i) in
    let layer_output = Array.map t.act_fn layer_output in
    values := layer_output
  done;
  !values

let train t inputs targets =
  let outputs = feedforward t inputs in
  let errors = ref (La.sub_vec_vec targets outputs) in
  La.vec_print_with_label inputs "inputs";
  La.vec_print_with_label targets "targets";
  La.vec_print_with_label outputs "outputs";
  La.vec_print_with_label !errors "errors";
  for i = Array.length t.weights - 1 downto 0 do
    La.mat_print_with_label t.weights.(i) (Printf.sprintf "weights_%i" i);
    (* We have to compute the weighted error from each output node. Since each row r in the weights
       corresponds to an output, and each column c to a hidden node,
       the error(c) = sum_over_every_r ( w_rc / sum_of_row(r) * error_r ) *)
    let incoming_node_count = La.(t.weights.(i).cols) in
    let back_errors = Array.init incoming_node_count (fun _ -> 0.0) in
    for c = 0 to incoming_node_count - 1 do
      for r = 0 to Array.length !errors - 1 do
        let sum_of_row = La.mat_fold_left_row ( +. ) r 0.0 t.weights.(i) in
        let w_rc = La.(t.weights.(i) $. (r, c)) in
        back_errors.(c) <- back_errors.(c) +. (w_rc /. sum_of_row *. !errors.(r))
      done
    done;
    La.vec_print_with_label back_errors (Printf.sprintf "errors_%i" i);
    errors := back_errors
  done;
  !errors
