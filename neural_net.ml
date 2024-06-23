module La = Linalg

type t = {
  (* We use an array of matrices because we have multiple layers *)
  weights : La.matrix array;
  biases : La.vector array;
  act_fn : float -> float;
}

let sigmoid v = 1.0 /. (1.0 +. exp v)

let create input_nodes hidden_nodes output_nodes =
  let rand () = Random.float 2.0 -. 1.0 in
  let weights =
    [|
      (* Rows = nodes, columns = edges going into the node *)
      La.mat_init hidden_nodes input_nodes (fun _ _ -> rand ());
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

let guess t inputs =
  let output_node_count = Array.length t.biases.(1) in
  (* TODO(Apaar): This can be made to work in-place with the array by modifying the Linalg funcs. Will just need the
     array to be long enough to accommodate every layer. *)
  let values = ref (Array.init output_node_count (fun i -> inputs.(i))) in
  for i = 0 to Array.length t.weights - 1 do
    let layer_output = La.mul_mat_vec t.weights.(i) !values in
    let layer_output = La.add_vec_vec layer_output t.biases.(i) in
    let layer_output = Array.map t.act_fn layer_output in
    values := layer_output
  done;
  !values
