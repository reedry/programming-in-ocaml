let count file =
  let file = open_in file in
  let rec count_rec line word byte is_char_before =
    try begin
      let c = input_char file in
      let is_char = not (c = '\n' || c = '\t' || c = ' ') in
      count_rec
        (if c = '\n' then line+1 else line)
        (if is_char_before && not is_char then word+1 else word)
        (byte+1)
        is_char
    end with
      End_of_file -> line, (if is_char_before then word+1 else word), byte
  in
  let l, w, b = count_rec 0 0 0 false in
  close_in file;
  l, w, b

let display_number_with_width n w =
  let len = String.length (string_of_int n) in
  let pad = String.make (w - len) ' ' in
  Printf.printf "%s%d " pad n

let display_count file disp_l disp_w disp_b =
  let l, w, b = count file in
  let width =
    let width_l = if disp_l then String.length (string_of_int l) else 0 in
    let width_w = if disp_w then String.length (string_of_int w) else 0 in
    let width_b = if disp_b then String.length (string_of_int b) else 0 in
    max (max width_l width_w) width_b
  in
  if disp_l then display_number_with_width l width;
  if disp_w then display_number_with_width w width;
  if disp_b then display_number_with_width b width;
  if not (disp_l || disp_w || disp_b) then begin
    let width = String.length (string_of_int b) in
    display_number_with_width l width;
    display_number_with_width w width;
    display_number_with_width b width
  end;
  print_endline file

let display_byte = ref false
let display_line = ref false
let display_word = ref false
let filenames = ref []

let spec = [("-c", Arg.Set display_byte, "Display only byte count");
            ("-l", Arg.Set display_line, "Display only line count");
            ("-w", Arg.Set display_word, "Display only word count");]

let () =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: wc [-l] [-w] [c] filename ...";
  match !filenames with
    [] -> exit 1
  | x :: _ -> display_count x !display_line !display_word !display_byte
