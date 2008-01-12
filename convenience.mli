type numtype = BP | PT | CM | MM | IN

val draw : 
    ?style:Path.style -> ?cycle:bool -> ?scale:numtype -> 
      (float * float) list -> Path.command
