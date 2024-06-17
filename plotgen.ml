(* Interactive plots by generating HTML/JS *)

type dataset = {
  (* TODO(Apaar): Use variant for this *)
  type_ : string;
  label : string;
  (* Array of x, y *)
  data : (float * float) array;
  background_color : string;
}

type t = {
  id : string;
  desc : string option;
  datasets : dataset array;
  x_min : float option;
  y_min : float option;
  x_max : float option;
  y_max : float option;
}

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
  for i = 0 to Array.length t.datasets - 1 do
    let ds = t.datasets.(i) in
    write_fn
    @@ Printf.sprintf "{ type: '%s',label: '%s',data: [" ds.type_ ds.label;
    for pi = 0 to Array.length ds.data - 1 do
      let x, y = ds.data.(pi) in
      write_fn @@ Printf.sprintf "{ x: %f, y: %f }," x y
    done;
    write_fn @@ Printf.sprintf "],backgroundColor: '%s'}," ds.background_color
  done;
  write_fn "]}"

let min_max_js min_opt max_opt =
  match (min_opt, max_opt) with
  | None, None -> ""
  | Some a, None -> Printf.sprintf "min: %f," a
  | None, Some b -> Printf.sprintf "max: %f," b
  | Some a, Some b -> Printf.sprintf "min: %f, max: %f, " a b

let config_js t write_fn =
  write_fn "{ data: ";
  data_js t write_fn;
  let min_max_x = min_max_js t.x_min t.x_max in
  let min_max_y = min_max_js t.y_min t.y_max in
  write_fn
  @@ Printf.sprintf
       ", options: { scales: { x: { type: 'linear', position: 'bottom', %s }, \
        y: { %s }}}}"
       min_max_x min_max_y

let postamble = {|
    )
  }
</script>
|}

let write t write_fn =
  write_fn (preamble t.id t.desc);
  config_js t write_fn;
  write_fn postamble

let write_oc t oc =
  let write_fn s = output_string oc s in
  write t write_fn
