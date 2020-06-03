let display_line_with width line =
  String.iteri
    (fun i c -> print_char c; if (i+1) mod width = 0 then print_newline ())
    line;
  let len = String.length line in
  if len mod width <> 0 || len = 0 then print_newline ()

let display_file width file =
  let file = open_in file in
  let rec display_line () =
    try begin
      let line = input_line file in
      ignore (display_line_with width line);
      display_line ()
    end with
      End_of_file -> ()
  in
  display_line ();
  close_in file


let line_width = ref 80
let filenames = ref []

let spec = [("--width", Arg.Int (fun n -> line_width := n), "line width")]

let () =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-n] filename ...";
  match !filenames with
    [] -> exit 1
  | x :: _ -> display_file !line_width x
