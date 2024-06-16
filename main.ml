let () =
  Random.self_init ();
  let p = Percep.init_rand 2 in
  Printf.printf "percep = %f\n" (Percep.guess p [| -1.0; 0.5 |])

let data = Yx_classifier_data.init_rand 100

let above_points =
  let open Yx_classifier_data in
  List.filter (fun p -> p.label = `Above) data
  |> List.map (fun p -> (p.x, p.y))
  |> Array.of_list

let below_points =
  let open Yx_classifier_data in
  List.filter (fun p -> p.label = `Below) data
  |> List.map (fun p -> (p.x, p.y))
  |> Array.of_list

let () =
  let f = open_out "index.html" in
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
    write_oc plot f);
  close_out f
