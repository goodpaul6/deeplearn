(* Bitmap file utilities because I want to plot stuff
   and see it. *)

type t = { w : int; h : int; data : bytes }

let bytes_per_pixel = 3
let create w h = { w; h; data = Bytes.create (w * h * bytes_per_pixel) }

let fill_all t r g b =
  let b =
    [ char_of_int r; char_of_int g; char_of_int b ]
    |> List.to_seq |> Bytes.of_seq
  in
  for i = 0 to (t.w * t.h) - 1 do
    Bytes.blit b 0 t.data (i * bytes_per_pixel) bytes_per_pixel
  done

let create_filled w h r g b =
  let bmp = create w h in
  fill_all bmp r g b;
  bmp

let stride t = t.w * bytes_per_pixel

let pos_to_idx t x y =
  let s = stride t in
  let idx = (y * s) + (x * bytes_per_pixel) in
  idx

let get t x y =
  let idx = pos_to_idx t x y in
  let r, g, b =
    Bytes.(get t.data idx, get t.data (idx + 1), get t.data (idx + 2))
  in
  (int_of_char r, int_of_char g, int_of_char b)

let plot t x y r g b =
  let idx = pos_to_idx t x y in
  Bytes.(set t.data idx (char_of_int r));
  Bytes.(set t.data (idx + 1) (char_of_int g));
  Bytes.(set t.data (idx + 2) (char_of_int b))

let hdr_size = 14
let info_size = 40
let hdr_info_size = hdr_size + info_size

(* Assumes i is big-endian in memory, which it is *)
let output_le_int oc i =
  output_byte oc (i land 0xff);
  output_byte oc ((i lsr 8) land 0xff);
  output_byte oc ((i lsr 16) land 0xff);
  output_byte oc ((i lsr 24) land 0xff)

(* Mostly translated from: https://stackoverflow.com/a/18675807 *)
let write t oc =
  (* 'BM' *)
  output_byte oc (int_of_char 'B');
  output_byte oc (int_of_char 'M');
  let s = stride t in
  let row_padding = (4 - (s mod 4)) mod 4 in
  let bitmap_data_size = (s + row_padding) * t.h in
  let total_size = hdr_info_size + bitmap_data_size in
  (* size in bytes *)
  output_le_int oc total_size;
  (* reserved *)
  output_le_int oc 0;
  (* start of data offset *)
  output_le_int oc hdr_info_size;
  (* info size *)
  output_le_int oc info_size;
  (* width *)
  output_le_int oc t.w;
  (* height *)
  output_le_int oc t.h;
  (* number of color planes *)
  output_byte oc 1;
  output_byte oc 0;
  (* bits per pixel *)
  output_byte oc 24;
  output_byte oc 0;
  (* compression level: none *)
  output_le_int oc 0;
  (* image data size *)
  output_le_int oc bitmap_data_size;
  (* horiz resolution in pixels / m *)
  output_byte oc 0x13;
  output_byte oc 0x0B;
  output_byte oc 0;
  output_byte oc 0;
  (* vert reso: 0x0B13 means 72 dpi *)
  output_byte oc 0x13;
  output_byte oc 0x0B;
  output_byte oc 0;
  output_byte oc 0;
  (* colors in palette: none, because we're using 24 bits per pixel *)
  output_le_int oc 0;
  (* important colors: none *)
  output_le_int oc 0;
  (* bitmap data *)
  for y = t.h - 1 downto 0 do
    for x = 0 to t.w - 1 do
      let r, g, b = get t x y in
      (* color order is bgr *)
      output_byte oc b;
      output_byte oc g;
      output_byte oc r
    done;
    for _ = 0 to row_padding - 1 do
      output_byte oc 0
    done
  done
