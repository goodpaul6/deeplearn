let percep =
  Random.self_init ();
  Percep.init_rand 2

let print_weights p =
  print_string "percep_weights = ";
  Array.iter (fun v -> Printf.printf "%f," v) Percep.(p.weights);
  print_newline ()

let () = Printf.printf "percep = %f\n" (Percep.guess percep [| -1.0; 0.5 |])
let points = Yx_classifier_data.init_rand 100

let split_by_label pts =
  let open Yx_classifier_data in
  let above = List.filter (fun p -> p.label > 0.0) pts in
  let below = List.filter (fun p -> p.label < 0.0) pts in
  let to_tuple p = (p.x, p.y) in
  ( above |> List.map to_tuple |> Array.of_list,
    below |> List.map to_tuple |> Array.of_list )

let plot_labeled_points points chart_id desc oc =
  let above_points, below_points = split_by_label points in
  Plotgen.(
    let plot =
      Scatter
        {
          desc;
          datasets =
            [|
              {
                label = "Above";
                points = above_points;
                background_color = "rgb(99, 255, 132)";
              };
              {
                label = "Below";
                points = below_points;
                background_color = "rgb(255, 99, 132)";
              };
            |];
        }
    in
    write_oc plot chart_id oc)

let train_on_all_points () =
  let open Yx_classifier_data in
  List.iter (fun v -> Percep.train percep [| v.x; v.y |] v.label) points;
  print_weights percep

let guess_points () =
  List.fold_left
    (fun acc v ->
      let open Yx_classifier_data in
      let x = v.x in
      let y = v.y in
      let guess = Percep.guess percep [| x; y |] in
      { x; y; label = guess } :: acc)
    [] points

let () =
  let f = open_out "index.html" in
  let guessed = guess_points () in
  for _ = 0 to 20 do
    train_on_all_points ()
  done;
  let trained_guesses = guess_points () in
  plot_labeled_points points "training_data" None f;
  plot_labeled_points guessed "guesses" None f;
  plot_labeled_points trained_guesses "trained_guesses"
    (Some "Ran 20 iterations of training") f;
  close_out f
