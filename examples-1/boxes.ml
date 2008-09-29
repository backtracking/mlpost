open Num
open Command
open Helpers
open Path
open Point
open Box

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

(*parse <<f1 *)
let f1 =
  let b = 
    hbox ~padding:(bp 20.)
      [vbox ~padding:(bp 4.) ~pos:`Right 
	 [tex "A"; tex ~name:"bc" "BC"; tex "D"];
       vbox ~padding:(bp 4.) ~pos:`Left  
	 [tex ~name:"e" "E"; tex "FGH"]]
  in
  [draw ~debug:false b;
   box_arrow (get "bc" b) (get "e" b)]

(*parse >> <<f2 *)
let f2 =
  let tex = tex ~style:Circle in
  let b = vbox [tex "a"; hbox [tex ~name:"b" "b"; tex "c"]] in
  let f = hbox ~padding:(bp 20.) [b;b;b] in
  let arrow = box_arrow ~outd:(vec (dir (-60.))) in
  let node i = get "b" (nth i f) in
  [draw ~debug:false f;
   arrow (node 0) (node 1); arrow (node 1) (node 2)]
(*parse >> *)

let () = Metapost.emit "f1" f1
let () = Metapost.emit "f2" f2
