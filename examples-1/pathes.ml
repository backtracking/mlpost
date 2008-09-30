open Path

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
let path1 = [ Command.draw (path ~style:jLine ~scale:Num.cm l)  ]

(*parse >> <<path2 *)
let path2 = [ Command.draw (path ~style:jLine ~scale:Num.cm ~cycle:jLine l)  ]
(*parse >> *)


let () = Metapost.emit "path1" path1
let () = Metapost.emit "path2" path2
