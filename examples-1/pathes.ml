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

(*parse <<path1 *)
let path1 = 
  let l = [0.,0.; 1., 0.; 0., 1.] in
    [ Command.draw (path ~style:jLine ~scale:Num.cm l)  ]
(*parse >> *)



let () = Metapost.emit "path1" path1
