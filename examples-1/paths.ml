open Path
open Command
open Point
open Dash

(*html
<script type="text/javascript">
<!--
    function toggle_visibility(id) {
       var e = document.getElementById(id);
       if(e.style.display == 'block')
          e.style.display = 'none';
       else
          e.style.display = 'block';
    }
//-->
</script>
*)

let l = [0.,0.; 1., 0.; 0., 1.]

(*parse <<path1 *)
let path1 = [ draw (path ~style:jLine ~scale:Num.cm l)  ]

(*parse >> <<path2 *)
let path2 = [ draw (path ~style:jLine ~scale:Num.cm ~cycle:jLine l)  ]

(*parse >> <<path3 *)
let path3 =
  let a =  -1. , -1. in
  let b =  1. , -1. in
  let c =  1. , 1. in
  let d =  -1. , 1. in
    [ draw (path ~style:jLine ~scale:Num.cm ~cycle:jLine [a;b;c;d]) ;
      draw (path ~scale:Num.cm [a;c]);
      draw (path ~scale:Num.cm [b;d]);
      ]

(*parse >> <<path4 *)
let path4 =
  [ draw ~pen:(Pen.circle ~tr:[Transform.scaled (Num.bp 4.)] ()) 
      (path [(0.,0.)])]

(*parse >> <<path5 *)
let path5 =
  let a = 0., 0. in
  let b = 1., 0. in
  let c = 0., 1. in
  let pen = (Pen.circle ~tr:[Transform.scaled (Num.bp 4.)] ()) in
  [ draw (path ~style:jLine ~scale:Num.cm ~cycle:jLine [a;b;c]) ] @
    List.map (fun a -> draw ~pen (path ~scale:Num.cm [a])) [a;b;c]
(*parse >> *)

let a = cmp (0., 0.)
let b = cmp (1., 0.)
let c = cmp (0., 1.)

(*html <hr /> *)

(*parse <<path6 *)
let path6 = 
    [draw (pathp ~style:jLine ~cycle:jLine [a;b;c]) ;
     draw (pathp [segment 0.5 a b ; c]) ;
     draw (pathp [segment 0.5 b c ; a]) ;
     draw (pathp [segment 0.5 c a ; b]) ; ]

(*parse >> <<path7 *)
let path7 = 
    [draw (pathp ~style:jLine [b;c;a]) ;
     draw (pathp ~style:jLine [a;b]) ~color:Color.yellow; ]

(*parse >> <<path8 *)
let path8 = 
    [ draw (pathp [a;b]);
      draw ~dashed:evenly (pathp [b;c]);
      draw ~dashed:withdots (pathp [c;a]) ]

(*parse >> <<path9 *)
let path9 = 
  let pen = Pen.circle ~tr:[Transform.scaled (Num.bp 2.)] () in
  let cl = List.map Color.gray [0.8;0.6;0.4] in
    List.map2
      (fun (a,b) color ->
         draw ~pen ~color (pathp ~style:jLine [a;b])) 
      [a,b;b,c;c,a] cl

(*parse >> <<path10 *)
let path10 = 
  [draw ~dashed:(scaled 2. evenly) (path ~scale:Num.cm [0.,0.; 3.,0.]) ;
   draw ~dashed:evenly (path ~scale:Num.mm [0.,-5.; 30.,-5.]) ]

(*parse >> <<path11 *)
let path11 = 
  [draw ~dashed:(pattern [on (Num.bp 2.) ; off (Num.bp 3.)] ) 
     (path ~scale:Num.cm [0.,0.; 3.,0.]) ; ]

(*parse >> *)
let triangle = path ~scale:Num.cm ~style:jLine ~cycle:jLine l
(*html <hr/> *)

(*parse <<path12 *)
let path12 = 
  [Command.fill ~color:(Color.gray 0.8) triangle]

(*parse >> <<path13 *)
let path13 =
    [ Command.fill ~color:(Color.gray 0.8) triangle; 
      Command.draw triangle]

(*parse >> *)
let pen = Pen.circle ~tr:[Transform.scaled (Num.bp 2.)] ()
(*html <hr/> *)
(*parse <<path14 *)
let path14 =
    [Command.fill ~color:(Color.gray 0.8) triangle; 
     Command.draw ~pen triangle]

(*parse >> <<path15 *)
let path15 =
    [ Command.draw ~pen triangle; 
      Command.fill ~color:(Color.gray 0.8) triangle]
(*parse >> *)

let _ = 
  List.iter (fun (name,fig) -> Metapost.emit name fig)
    [ "path1", path1 ;
      "path2", path2;
      "path3", path3;
      "path4", path4;
      "path5", path5;
      "path6", path6;
      "path7", path7;
      "path8", path8;
      "path9", path9;
      "path10", path10;
      "path11", path11;
      "path12", path12;
      "path13", path13;
      "path14", path14;
      "path15", path15;
    ]
