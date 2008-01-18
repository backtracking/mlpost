type numtype = BP | PT | CM | MM | IN

val draw : 
    ?style:Path.joint -> ?cycle:bool -> ?scale:numtype -> 
      (float * float) list -> Path.command
