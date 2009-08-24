open Mlpost
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
  seq [draw b; arrow "A" "B"]

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
  seq [draw b;
       box_arrow (get "bc" b) (get "e" b)]

(*parse >> <<f2 *)
let f2 =
  let tex = tex ~style:Circle ~stroke:(Some Color.black) in
  let b = vbox [tex "a"; hbox [tex ~name:"b" "b"; tex "c"]] in
  let f = hbox ~padding:(bp 20.) [b;b;b] in
  let arrow = box_arrow ~outd:(vec (dir (-60.))) in
  let node i = get "b" (nth i f) in
  seq [draw f;
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
  draw b

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
  draw b

(*parse >> <<custom *)
module P = Point
open Num.Infix

let (|>) x f = f x

let draw_point t = Point.draw ~pen:(Pen.scale (bp 4.) Pen.default) ~color:(Color.red) t

(* align verticalement le barycentre [(west,5);(east,2)] *)
let custom =
  let two = Num.bp 2. in
  let five = Num.bp 5. in
  let tex = tex ~dx:two ~dy:two in
  let a = tex "recursively enumerable languages" in
  let b = tex "context sensitive" in
  let c = tex "context free" in
  let add_point t = 
    let w = corner `West t in
    let e = corner `East t in
    let p = P.mult (one // (two +/ five)) (P.add (P.mult five w) (P.mult two e)) in
    setp "something" p t in
  let a = add_point a in
  let b = add_point b in
  let c = add_point c in
  let points = [a;b;c]
    |> List.map (getp "something")
    |> List.map draw_point
    |> Command.seq  in
  (*(*Example de débuggage quand on a le nouveau backend*)
    List.iter fun p -> let {Concrete.CPoint.x=x;y=y} = Concrete.cpoint_of_point (getp "something" p) in
             Format.printf "x = %f; y = %f@." x y) [a;b;c];*)
  Command.seq [
    points;
  Box.draw (vbox ~pos:(`Custom (getp "something")) [a;b;c])]
  

(*parse >> <<place *)

let place =
  let b = rect (empty ~width: (cm 3.) ~height: (cm 1.5) ()) in
  let make t pos = rect (Box.place pos ~padding: (cm 1.) b (tex t)) in
  let boxes = [
    b;
    make "center" `Center;
    make "south" `South;
    make "north" `North;
    make "east" `East;
    make "west" `West;
    make "southwest" `Southwest;
    make "southeast" `Southeast;
    make "northwest" `Northwest;
    make "northeast" `Northeast;
  ] in
  Command.seq (List.map Box.draw boxes)

(*parse >> *)

let () = Metapost.emit "simple" simple
let () = Metapost.emit "f1" f1
let () = Metapost.emit "f2" f2
let () = Metapost.emit "traffic" traffic
let () = Metapost.emit "hierarchy" hierarchy
let () = Metapost.emit "custom" custom
let () = Metapost.emit "place" place
