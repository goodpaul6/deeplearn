type t = {
  label_data : Bytes.t;
  image_data : Bytes.t;
  image_count : int;
  image_rows : int;
  image_cols : int;
}

let read_labels_ic ic =
  let magic = input_binary_int ic in
  let size = input_binary_int ic in
  if magic <> 2049 then
    invalid_arg "File has invalid magic number for label data"
  else
    let buf = Bytes.create size in
    really_input ic buf 0 size;
    buf

let read_images_ic ic =
  let magic = input_binary_int ic in
  let size = input_binary_int ic in
  let rows = input_binary_int ic in
  let cols = input_binary_int ic in
  if magic <> 2051 then
    invalid_arg "File has invalid magic number for image data"
  else
    let len = size * rows * cols in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    (buf, size, rows, cols)

let with_file fn filename =
  let ic = open_in_bin filename in
  try fn ic
  with e ->
    close_in_noerr ic;
    raise e

let load_labels_file filename = with_file read_labels_ic filename
let load_images_file filename = with_file read_images_ic filename

let image_bytes data image_idx =
  let bytes_per_image = data.image_rows * data.image_cols in
  Bytes.sub data.image_data (bytes_per_image * image_idx) bytes_per_image

let load labels_filename images_filename =
  let label_data = load_labels_file labels_filename in
  let image_data, image_count, image_rows, image_cols =
    load_images_file images_filename
  in
  { label_data; image_data; image_count; image_rows; image_cols }
