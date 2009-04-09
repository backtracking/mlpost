let read_prelude_from_tex_file file = 
  let c = open_in file in
  let s = Scan_prelude.scan (Lexing.from_channel c) in
  close_in c;
  s
