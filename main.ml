let percep =
  Random.self_init ();
  (* 3 weights because 3rd one for the bias *)
  Percep.init_rand 3

let _print_weights p =
  print_string "percep_weights = ";
  Array.iter (fun v -> Printf.printf "%f," v) Percep.(p.weights);
  print_newline ()

let nn = Neural_net.create 2 2 1

let () =
  Printf.printf "percep = %f\n" (Percep.guess percep [| -1.0; 0.5; 1.0 |]);
  let res = Neural_net.feedforward nn [| 1.0; 0.0 |] in
  Array.iter (fun v -> Printf.printf "%f," v) res;
  print_newline ()

let _transposed = Linalg.mat_init 3 2 (fun _ _ -> 0.0) |> Linalg.mat_transpose
let line_f x = (0.3 *. x) +. 0.2

let points =
  Yx_classifier_data.init_rand 100 (fun x y ->
      (* y = 3x + 2 *)
      let line_y = line_f x in
      y >= line_y)

let split_by_label pts =
  let open Yx_classifier_data in
  let above = List.filter (fun p -> p.label > 0.0) pts in
  let below = List.filter (fun p -> p.label < 0.0) pts in
  let to_tuple p = (p.x, p.y) in
  ( above |> List.map to_tuple |> Array.of_list,
    below |> List.map to_tuple |> Array.of_list )

let guessed_line_y x =
  Percep.(
    let w0 = percep.weights.(0) in
    let w1 = percep.weights.(1) in
    let w2 = percep.weights.(2) in
    ((w0 *. x) +. w2) /. -.w1)

let plot_labeled_points ?(show_guessed_line = false) points chart_id desc oc =
  let above_points, below_points = split_by_label points in
  Plotgen.(
    let plot =
      {
        id = chart_id;
        desc;
        x_min = Some (-1.0);
        y_min = Some (-1.0);
        x_max = Some 1.0;
        y_max = Some 1.0;
        datasets =
          List.filter Option.is_some
            [
              Some
                {
                  type_ = "scatter";
                  label = "Above";
                  data = above_points;
                  background_color = "rgb(99, 255, 132)";
                };
              Some
                {
                  type_ = "scatter";
                  label = "Below";
                  data = below_points;
                  background_color = "rgb(255, 99, 132)";
                };
              Some
                {
                  type_ = "line";
                  label = "Line";
                  data = [| (-1.0, line_f (-1.0)); (1.0, line_f 1.0) |];
                  background_color = "rgb(132, 99, 255)";
                };
              (if show_guessed_line then
                 Some
                   {
                     type_ = "line";
                     label = "Guessed Line";
                     data =
                       [|
                         (-1.0, guessed_line_y (-1.0)); (1.0, guessed_line_y 1.0);
                       |];
                     background_color = "rgb(99, 255, 255)";
                   }
               else None);
            ]
          |> List.map Option.get |> Array.of_list;
      }
    in
    write_oc plot oc)

let train_on_all_points () =
  let open Yx_classifier_data in
  List.iter (fun v -> Percep.train percep [| v.x; v.y; 1.0 |] v.label) points

let guess_points () =
  List.fold_left
    (fun acc v ->
      let open Yx_classifier_data in
      let x = v.x in
      let y = v.y in
      (* 1.0 for the bias *)
      let guess = Percep.guess percep [| x; y; 1.0 |] in
      { x; y; label = guess } :: acc)
    [] points

let () =
  let f = open_out "index.html" in
  let guessed = guess_points () in
  for _ = 0 to 50 do
    train_on_all_points ()
  done;
  let trained_guesses = guess_points () in
  plot_labeled_points points "training_data" None f;
  plot_labeled_points guessed "guesses" None f;
  plot_labeled_points ~show_guessed_line:true trained_guesses "trained_guesses"
    (Some "Ran 50 iterations of training") f;
  close_out f
