let percep_f x = if x > 0.0 then 1.0 else 0.0
let _sig_f x = 1.0 /. (1.0 +. exp (-.x))

let nand_net =
  let weights =
    Mat.mat_of_arrays
      [| [| 1.0; 0.0; -2.0 |]; [| 0.0; 1.0; -2.0 |]; [| 0.0; 0.0; 1.0 |] |]
  in
  let biases = [| 0.0; 0.0; 3.0 |] in
  Neural_net.create weights biases percep_f

let example_nand_net = Neural_net.prop [| 1.0; 0.0; 0.0 |] nand_net
let example_nand_net_2 = Neural_net.prop [| 0.0; 1.0; 0.0 |] nand_net
let example_nand_net_3 = Neural_net.prop [| 1.0; 1.0; 0.0 |] nand_net
let print_label s = Printf.printf "%s = " s

let print_vec v =
  Array.iter (Printf.printf "%f ") v;
  print_newline ()

let _test_data =
  Mnist_data.load "mnist-data/t10k-labels.idx1-ubyte"
    "mnist-data/t10k-images.idx3-ubyte"

let () =
  print_label "example_nand_net";
  print_vec example_nand_net;
  print_label "example_nand_net_2";
  print_vec example_nand_net_2;
  print_label "example_nand_net_3";
  print_vec example_nand_net_3
