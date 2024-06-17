let percep =
  Random.self_init ();
  Percep.init_rand 2

let () =
  Array.iter (fun v -> Printf.printf "%f," v) percep.weights;
  print_newline ();
  Printf.printf "percep = %f\n" (Percep.guess percep [| -1.0; 0.5 |])

let points = Yx_classifier_data.init_rand 100

let split_by_label pts =
  let open Yx_classifier_data in
  let above = List.filter (fun p -> p.label = `Above) pts in
  let below = List.filter (fun p -> p.label = `Below) pts in
  let to_tuple p = (p.x, p.y) in
  ( above |> List.map to_tuple |> Array.of_list,
    below |> List.map to_tuple |> Array.of_list )

let plot_labeled_points points chart_id oc =
  let above_points, below_points = split_by_label points in
  Plotgen.(
    let plot =
      Scatter
        {
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

let () =
  let f = open_out "index.html" in
  let guessed =
    List.fold_left
      (fun acc v ->
        let open Yx_classifier_data in
        let x = v.x in
        let y = v.y in
        let guess = Percep.guess percep [| x; y |] in
        { x; y; label = (if guess > 0.0 then `Above else `Below) } :: acc)
      [] points
  in
  plot_labeled_points points "training_data" f;
  plot_labeled_points guessed "guesses" f;
  close_out f
