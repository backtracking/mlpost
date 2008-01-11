type style = Straight | Curved
type numtype = BP | PT | CM | MM | IN

val draw : 
    ?style:style -> ?cycle:bool -> ?scale:numtype -> 
      (float * float) list -> Path.command
