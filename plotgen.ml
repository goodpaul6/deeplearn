(* Interactive plots by generating HTML/JS *)

type scatter_dataset = {
  label : string;
  (* Array of x, y *)
  points : (float * float) array;
  background_color : string;
}

type t = Scatter of { desc : string option; datasets : scatter_dataset array }

let preamble chart_id desc =
  Printf.sprintf
    {|
<div style="padding: 100">
  <h1>%s</h1>
  %s
  <canvas id="%s"></canvas>
</div>

<script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
<script>
  {
    const ctx = document.getElementById('%s');
    new Chart(ctx, 
|}
    chart_id
    (match desc with Some s -> Printf.sprintf "<p>%s</p>" s | None -> "")
    chart_id chart_id

let data_js t write_fn =
  write_fn "{datasets: [";
  (match t with
  | Scatter { datasets; _ } ->
      for i = 0 to Array.length datasets - 1 do
        let ds = datasets.(i) in
        write_fn @@ Printf.sprintf "{ label: '%s',data: [" ds.label;
        for pi = 0 to Array.length ds.points - 1 do
          let x, y = ds.points.(pi) in
          write_fn @@ Printf.sprintf "{ x: %f, y: %f }," x y
        done;
        write_fn
        @@ Printf.sprintf "],backgroundColor: '%s'}," ds.background_color
      done);
  write_fn "]}"

let config_js t write_fn =
  write_fn "{";
  (match t with
  | Scatter _ ->
      write_fn "type: 'scatter',data: ";
      data_js t write_fn;
      write_fn
        ", options: { scales: { x: { type: 'linear', position: 'bottom' } } }");
  write_fn "}"

let postamble = {|
    )
  }
</script>
|}

let get_desc t = match t with Scatter { desc; _ } -> desc

let write t chart_id write_fn =
  let desc = get_desc t in
  write_fn (preamble chart_id desc);
  config_js t write_fn;
  write_fn postamble

let write_oc t chart_id oc =
  let write_fn s = output_string oc s in
  write t chart_id write_fn
