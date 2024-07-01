type vector = float array

(* Data is stored in a column-major format, meaning (row, col) is adjacent to (row, col + 1) *)
type matrix = { rows : int; cols : int; data : float array }

let ( $. ) mat (row, col) = mat.data.((row * mat.cols) + col)

let mat_init rows cols f =
  let data =
    Array.init (rows * cols) (fun i ->
        let r = i / cols in
        let c = i mod cols in
        f r c)
  in
  { rows; cols; data }

let mat_set mat row col value = mat.data.((row * mat.cols) + col) <- value

let mat_of_arrays arrs =
  assert (Array.length arrs > 0);
  let rows = Array.length arrs in
  let cols = Array.length arrs.(0) in
  mat_init rows cols (fun row col -> arrs.(col).(row))

let mat_transpose mat =
  mat_init mat.cols mat.rows (fun t_row t_col -> mat $. (t_col, t_row))

let mat_map mat f =
  mat_init mat.cols mat.rows (fun t_row t_col -> f (mat $. (t_col, t_row)))

let mul_mat_scalar mat scalar =
  mat_init mat.rows mat.cols (fun row col -> (mat $. (row, col)) *. scalar)

let mul_mat_vec mat vec =
  assert (Array.length vec = mat.cols);
  Array.init mat.rows (fun row ->
      let sum = ref 0.0 in
      for col = 0 to mat.cols - 1 do
        sum := !sum +. ((mat $. (row, col)) *. vec.(col))
      done;
      !sum)

let mul_mat_mat a b =
  assert (a.cols = b.rows);
  let c = mat_init a.rows b.cols (fun _ _ -> 0.0) in
  for i = 0 to a.rows - 1 do
    for j = 0 to b.cols - 1 do
      let sum = ref 0.0 in
      for k = 0 to a.cols - 1 do
        sum := !sum +. ((a $. (i, k)) *. (b $. (k, j)))
      done;
      mat_set c i j !sum
    done
  done;
  c

let add_mat_mat a b =
  assert (a.rows = b.rows);
  assert (a.cols = b.cols);
  mat_init a.rows a.cols (fun row col -> (a $. (row, col)) +. (b $. (row, col)))

let add_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))
let sub_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) -. b.(i))
let mul_vec_vec a b = Array.init (Array.length a) (fun i -> a.(i) *. b.(i))
let mul_vec_scalar a s = Array.init (Array.length a) (fun i -> a.(i) *. s)

let vec_to_row_mat vec =
  mat_init 1 (Array.length vec) (fun _row col -> vec.(col))

let vec_to_col_mat vec =
  mat_init (Array.length vec) 1 (fun row _col -> vec.(row))

let dot_vec_vec a b =
  let sum = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    sum := !sum +. (a.(i) *. b.(i))
  done;
  !sum

let mat_print t =
  for i = 0 to t.rows - 1 do
    print_string "| ";
    for j = 0 to t.cols - 1 do
      Printf.printf "%f " (t $. (i, j))
    done;
    print_string "|\n"
  done

let vec_print t =
  print_string "| ";
  for i = 0 to Array.length t - 1 do
    Printf.printf "%f " t.(i)
  done;
  print_string "|\n"

let mat_print_with_label t label =
  Printf.printf "%s =\n" label;
  mat_print t

let vec_print_with_label t label =
  Printf.printf "%s = " label;
  vec_print t
