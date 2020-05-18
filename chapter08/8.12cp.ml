let cp src dest =
  let src = open_in src and dest = open_out dest in
  let rec write_byte () =
    try begin
      output_byte dest (input_byte src);
      write_byte ()
    end with End_of_file -> ()
  in
  write_byte ();
  close_in src;
  close_out dest
