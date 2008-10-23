open Num
open Command
open Helpers
open Path
open Point
open Color
open Box

(*parse <<togglescript>> *)

(*parse <<simple *)

let simple =
  let node s =
    rect ~name:s ~stroke:None (round_rect ~stroke:None ~fill:lightblue (tex s))
  in
  let b = hbox ~padding:(bp 20.) [node "A"; node "B"] in
  let arrow x y = box_arrow ~pen:Pen.circle ~color:red (get x b) (get y b) in
  [draw b;
   arrow "A" "B"]

(*parse >> <<f1 *)
let f1 =
  let tex = tex ~style:Rect ~stroke:(Some Color.black) in
  let b = 
    hbox ~padding:(bp 20.)
      [vbox ~padding:(bp 4.) ~pos:`Right 
        [tex "A"; tex ~name:"bc" "$\\sqrt{x}$"; tex "D"];
       vbox ~padding:(bp 4.) ~pos:`Left  
	 [tex ~name:"e" "E"; tex "FGH"]]
  in
  [draw ~debug:false b;
   box_arrow (get "bc" b) (get "e" b)]

(*parse >> <<f2 *)
let f2 =
  let tex = tex ~style:Circle ~stroke:(Some Color.black) in
  let b = vbox [tex "a"; hbox [tex ~name:"b" "b"; tex "c"]] in
  let f = hbox ~padding:(bp 20.) [b;b;b] in
  let arrow = box_arrow ~outd:(vec (dir (-60.))) in
  let node i = get "b" (nth i f) in
  [draw ~debug:false f;
   arrow (node 0) (node 1); arrow (node 1) (node 2)]

(*parse >> <<traffic *)
(* inspired by functional metapost *)
let traffic =
  let two = Num.bp 2. in
  let b = 
    vbox ~fill:black ~padding:(Num.bp 3.) ~dx:two ~dy:two
      [ tex ~style:Circle ~fill:red "R";
        tex ~style:Circle ~fill:yellow "Y";
        tex ~style:Circle ~fill:green "G"; ]
  in
  [ draw b]

(*parse >> <<hierarchy *)
(* inspired by functional metapost *)
let hierarchy =
  let two = Num.bp 2. in
  let five = Num.bp 5. in
  let tex = tex ~dx:two ~dy:two in
  let vbox = vbox ~padding:(Num.bp 3.) ~stroke:(Some black) 
                  ~style:RoundRect ~dy:five ~dx:five
  in
  let b = 
      vbox [ tex "recursively enumerable languages";
        vbox [ tex "decidable languages";
          vbox [ tex "context sensitive";
            vbox [tex "context free"; 
                  tex ~style:RoundRect ~stroke:(Some black) "regular" ] 
      ] ] ]
  in
  [ draw b]
(*parse >> *)


let () = Metapost.emit "simple" simple
let () = Metapost.emit "f1" f1
let () = Metapost.emit "f2" f2
let () = Metapost.emit "traffic" traffic
let () = Metapost.emit "hierarchy" hierarchy
