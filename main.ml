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
  let f = open_out "index.html" in
  Plotgen.(
    let plot =
      Scatter
        {
          datasets =
            [|
              {
                label = "Test";
                points =
                  [| (-10.0, 0.0); (0.0, 10.0); (10.0, 5.0); (0.5, 5.5) |];
                background_color = "rgb(255, 99, 132)";
              };
            |];
        }
    in
    write_oc plot f);
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
