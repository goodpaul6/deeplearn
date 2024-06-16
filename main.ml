let percep_f x = if x > 0.0 then 1.0 else 0.0
let _sig_f x = 1.0 /. (1.0 +. exp (-.x))

let nand_net =
  let weights =
    Linalg.mat_of_arrays
      [| [| 1.0; 0.0; -2.0 |]; [| 0.0; 1.0; -2.0 |]; [| 0.0; 0.0; 1.0 |] |]
  in
  Neural_net.create weights [| 0.0; 0.0; 3.0 |] percep_f

let example_dot =
  Linalg.mul_mat_vec
    (Linalg.mat_of_arrays
       [| [| 2.0; 0.0; 0.0 |]; [| 0.0; 2.0; 0.0 |]; [| 0.0; 0.0; 2.0 |] |])
    [| 1.0; 1.0; 1.0 |]

let example_add = Linalg.add_vec_vec [| 1.0; 0.0; 0.0 |] [| 1.0; 1.0; -1.0 |]
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
  Random.self_init ();
  let p = Percep.init_rand 2 in
  Printf.printf "percep = %f\n" (Percep.guess p [| -1.0; 0.5 |])

let () =
  let f = open_out_bin "test.bmp" in
  let bmp = Bmp.create_filled 100 100 0xff 0xff 0xff in
  Bmp.fill_rect bmp 0 0 50 50 0xff 0 0;
  Bmp.plot_line bmp 0 0 50 50 0 0xff 0;
  Bmp.write bmp f;
  close_out f

let () =
  print_label "example_dot";
  print_vec example_dot;
  print_label "example_add";
  print_vec example_add;
  print_label "example_nand_net";
  print_vec example_nand_net;
  print_label "example_nand_net_2";
  print_vec example_nand_net_2;
  print_label "example_nand_net_3";
  print_vec example_nand_net_3
