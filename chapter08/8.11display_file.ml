let display_file file =
  let file = open_in file in
  let rec display_line n =
    try begin
      let line = input_line file in
      print_endline (string_of_int n ^ "  " ^ line);
      display_line (n+1)
    end with
      End_of_file -> ()
  in
  display_line 1;
  close_in file
