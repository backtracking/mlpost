open Int32

type t = Int32.t
let init = of_int 55665
let c1 = of_int 52845
let c2 = of_int 22719
let max16 = of_int 65536
let mask16 = of_int 65535
let random_bytes = 4

let enc_int r plain =
  let er = !r in
  let cipher = logxor plain (shift_right_logical !r 8) in
  let tmp2 = add (mul (add cipher !r) c1) c2 in
  r := logand tmp2 mask16 (*rem tmp2 max16*);
  Format.printf "ENC : @[tmp2 = %li@.er = %li@.cipher = %li@.plain = %li@]@." tmp2 er cipher plain;
  cipher

let dec_int r cipher =
  (*let er = !r in*)
  let plain =  logxor cipher (shift_right_logical !r 8) in
  let tmp2 = add (mul (add cipher !r) c1) c2 in
  r := logand tmp2 mask16 (*rem tmp2 max16*);
  (*Format.printf "DEC : @[tmp2 = %li@.r = %li@.cipher = %li@.plain = %li@]@." tmp2 er cipher plain;*)
  plain

let dec_char r cipher =
  let cipher = of_int (int_of_char cipher) in
  let plain = dec_int r cipher in
  char_of_int (to_int plain)

let dec_string r scipher =
  for i=0 to String.length scipher - 1 do
    scipher.[i] <- dec_char r scipher.[i]
  done

let dec_channel_for_lexer ic =
  let r = ref init in
  let s = String.make random_bytes ' ' in
  ignore (input ic s 0 random_bytes);
  dec_string r s;
  Lexing.from_function (fun buf n -> 
                          let n = input ic buf 0 n in
                          dec_string r buf;
                          n)

let rc_start = "RD"

let to_suppres = ref 0

let suppres buf_out buf_tmp start len =
  Buffer.add_substring buf_out buf_tmp 0 start;
  let buflen = String.length buf_tmp in
  if len+start <buflen then
    (Buffer.add_substring buf_out buf_tmp (start+len) 
      (buflen-start-len);
     0)
  else
    len - (buflen + 1 - start)

let eexec_line buf_tmp buf_out =
  if !to_suppres != 0 then
    to_suppres := suppres buf_out buf_tmp 0 !to_suppres
  else
    begin
      Buffer.add_char buf_out '\n';
      let scanf_f n i n2 =
        to_suppres := suppres buf_out buf_tmp n (n2-n +i) in
      let tmp = Scanf.Scanning.from_string buf_tmp in
      try
        Scanf.bscanf tmp "dup %_i %n%i RD %n" scanf_f;
      with Scanf.Scan_failure _ | End_of_file ->
        (*Printf.printf "Scanf dup : %s\n" s;*)
        try
          Scanf.bscanf tmp "%_s %n%i RD %n" scanf_f;
        with Scanf.Scan_failure _ | End_of_file->
          (*Printf.printf "Scanf other : %s\n" s;*)
          Buffer.add_string buf_out buf_tmp
    end

let dec_buffer ch len buf_out =
  let buf_tmp = Buffer.create 30 in
  let r = ref init in
  for i=0 to len -1 do
    let dec = (dec_char r (input_char ch)) in
    if dec == '\013' then Buffer.add_char buf_tmp '\n'
    else if dec != '\n' then Buffer.add_char buf_tmp dec
    else 
      (eexec_line (Buffer.contents buf_tmp) buf_out;
       Buffer.clear buf_tmp)
  done




let all ch = 
  let r = ref init in
  while true do
    let c = input_char ch in
    let dc = dec_char r c in
    Format.printf "(%c,%i)  -> (%c,%i)@." c (int_of_char c) dc (int_of_char dc)
  done

let rec input_char_list ch = function
  | 0 -> []
  | n -> let c = input_char ch in
    c::(input_char_list ch (n-1))

let rec decale  = function
  | [] -> ()
  | _::tl as l -> 
      let r = ref init in
      let l = List.map (dec_char r) l in
      Format.printf "LINE : %i\n" (List.length l);
      Format.printf "%a@.@." (fun fmt -> List.iter (Format.fprintf fmt "%c")) l;
      decale tl

let search ch = 
  let l = input_char_list ch 100 in
  decale l

let show ch =
  while true do
    let c = input_char ch in
    Format.printf "(%c,%i);\n" c (int_of_char c)
  done

let input_binary_int_little ch =
  let f x n = logor x (shift_left (of_int (int_of_char (input_char ch))) n) in
  to_int (f (f (f (f zero 0) 8) 16) 24)

type pfb_marker =
  | PFB_MARKER
  | PFB_ASCII
  | PFB_BINARY
  | PFB_DONE
  | NOTPFB of char

let conv_marker = function
  | '\001' -> PFB_ASCII
  | '\002' -> PFB_BINARY
  | '\003' -> PFB_DONE
  | '\128' -> PFB_MARKER
  | c -> NOTPFB c



let find_block ch =
  let buf_out = Buffer.create 1000 in
  let rec find_block_aux () =
    let c = conv_marker (input_char ch) in
    let blocktyp = conv_marker (input_char ch) in
    match c,blocktyp with
      | PFB_MARKER, PFB_DONE -> ()
      | PFB_MARKER, PFB_ASCII ->
          let block_len = input_binary_int_little ch in
          for i=0 to block_len -1 do
            let c = input_char ch in
            if c == '\013' then Buffer.add_char buf_out '\n'
            else Buffer.add_char buf_out c
          done;
          find_block_aux ()
      | PFB_MARKER, PFB_BINARY ->
          let block_len = input_binary_int_little ch in
          dec_buffer ch block_len buf_out;
          find_block_aux ()
      | _ -> failwith "pfb format error, try with t1disasm"
  in 
  find_block_aux ();
  buf_out

let open_decr filename =
  let ch = open_in_bin filename in
  find_block ch

let print_block ch =
    Buffer.output_buffer stdout (find_block ch)
               
(*let _ = 
  let ch = open_in_bin Sys.argv.(1) in
  print_block ch
*)
