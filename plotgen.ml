(* Interactive plots by generating HTML/JS *)

type scatter_dataset = {
  label : string;
  (* Array of x, y *)
  points : (float * float) array;
  background_color : string;
}

type t = Scatter of { datasets : scatter_dataset array }

let preamble =
  {|
<div>
  <canvas id="chart"></canvas>
</div>

<script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
<script>
  const ctx = document.getElementById('chart');
  new Chart(ctx, 
|}

let data_js t write_fn =
  write_fn "{datasets: [";
  (match t with
  | Scatter { datasets } ->
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
</script>
|}

let write t write_fn =
  write_fn preamble;
  config_js t write_fn;
  write_fn postamble

let write_oc t oc =
  let write_fn s = output_string oc s in
  write t write_fn
