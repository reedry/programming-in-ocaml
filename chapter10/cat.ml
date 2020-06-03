let display_file display_num file =
  let file = open_in file in
  let rec display_line n =
    try begin
      let line = input_line file in
      if display_num then Printf.printf "%6d\t" n;
      print_endline line;
      display_line (n+1)
    end with
      End_of_file -> ()
  in
  display_line 1;
  close_in file

let display_linenum = ref false
let filenames = ref []

let spec = [("-n", Arg.Set display_linenum, "Display line number")]

let () =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-n] filename ...";
  match !filenames with
    [] -> exit 1
  | x :: _ -> display_file !display_linenum x
