open FrameWork

let test_from_string =
  Test.mk ~name:"from_string" (fun () ->
    let s = "toto.mps" in
    let f = File.from_string s in
    Assert.eq ~s:"toto.mps" (File.to_string f) "toto.mps";
    let p = File.set_ext f "pdf" in
    Assert.eq ~s:"toto.pdf" (File.to_string p) "toto.pdf")

let tests =
  [ test_from_string;
  ]
