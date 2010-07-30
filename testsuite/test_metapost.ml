let s = "hello"
let fig = Picture.tex s
module SM = Misc.StringMap

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
  let id_unit () = ()

  type t =
    { prepare : unit -> unit;
      run : unit -> unit;
      clean_up : unit -> unit }

  let mk ?(prepare=id_unit) ?(clean_up=id_unit) run =
    { prepare = prepare ; clean_up = clean_up; run = run }

  let run_one t =
    t.prepare ();
    begin try t.run (); Format.printf ".@?"
    with
    | Assert.Assert _ -> Format.printf "!@?"
    | _ -> Format.printf "?@?"
    end;
    t.clean_up ()

  let run_many l =
    List.iter run_one l;
    Format.printf "@."

end

let test_generate_mp =
  let out = "hello.output" in
  let ref = "hello.reference" in
  Test.mk (fun () ->
    Metapost.generate_mp out [1,fig];
    Assert.File.exists out;
    Assert.File.eq ref out
  )

let test_generate_aux =
  let out = "hello.1" in
  let ref = "hello.mps.reference" in
  let rename = SM.add out out SM.empty in
  Test.mk (fun () ->
    Metapost.generate_aux rename s [1,fig];
    Assert.File.exists out;
    Assert.File.eq ~ignore:"%%CreationDate:" ref out
  )


let tests =
  [ test_generate_mp ; test_generate_aux ]

let _ =
  Sys.chdir "testspace";
  Test.run_many tests
