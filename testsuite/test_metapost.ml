let s = "hello"
let fig = Picture.tex s
module FM = File.Map

module Assert = struct

  exception Assert of string option

  let assert_failure s = raise (Assert s)

  let bool ?s cond = if cond then () else assert_failure s
  let eq ?s a b = if a = b then () else assert_failure s

  module File = struct
    let exists ?s f = bool ?s (Sys.file_exists f)
    let eq ?s ?ignore a b =
      let ignore =
        match ignore with
        | None -> ""
        | Some s -> "-I " ^ s in
      let r =
        Misc.call_cmd ~outv:true (Misc.sprintf "diff %s %s %s" ignore a b) in
      eq ?s r 0
  end
end

module Test = struct

  let _ = Printexc.record_backtrace true

  let id_unit () = ()

  type t =
    { prepare : unit -> unit;
      run : unit -> unit;
      clean_up : unit -> unit;
      name : string
    }

  let mk ?(prepare=id_unit) ?(clean_up=id_unit) ~name run =
    { prepare = prepare ; clean_up = clean_up; run = run; name = name }

  let run_one t =
    t.prepare ();
    begin try t.run (); Format.printf ".@?"
    with
    | Assert.Assert _ -> Format.printf "!@?"
    | e ->
        Format.printf "?@?";
        Format.eprintf "Error during test %s...@." t.name;
        Format.eprintf "%s@." (Printexc.to_string e);
        Printexc.print_backtrace Pervasives.stderr
    end;
    try t.clean_up ()
    with e ->
      Format.eprintf "Error during cleanup of test %s...@." t.name;
      Format.eprintf "%s@." (Printexc.to_string e);
      Printexc.print_backtrace Pervasives.stderr;
      exit 1


  let run_many l =
    List.iter run_one l;
    Format.printf "@."

end

let test_generate_mp =
  let out = "hello.output" in
  let ref = "hello.reference" in
  Test.mk
    ~name:"generate_mp"
    ~clean_up:(fun () ->
      Sys.remove out)
    (fun () ->
      Metapost.generate_mp out [1,"",fig];
      Assert.File.exists out;
      Assert.File.eq ref out)

let test_generate_aux =
  let out = "hello.1" in
  let ref = "hello.mps.reference" in
  let outf = File.from_string out in
  let rename = FM.add outf outf FM.empty in
  Test.mk
    ~name:"generate_aux"
    ~clean_up:(fun () ->
      Sys.remove out)
    (fun () ->
      Metapost.generate_aux rename (File.from_string s) [1,"",fig];
      Assert.File.exists out;
      Assert.File.eq ~ignore:"%%CreationDate:" ref out)

let test_dump_png =
  let bn = "hello" in
  let out = bn ^ ".png" in
  Test.mk
    ~name:"dump_png"
    ~prepare:(fun () ->
      Metapost.emit bn fig;
      (* TODO this reset should not be necessary *)
      Compile.reset ())
    ~clean_up:(fun () ->
      Sys.remove out)
    (fun () ->
      Metapost.dump_png bn;
      Assert.File.exists out)


let tests =
  [ test_generate_mp ;
    test_generate_aux;
    test_dump_png
    ]

let _ =
  Sys.chdir "testspace";
  Test.run_many tests
