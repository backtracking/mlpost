val draw : 
    ?style:Path.joint -> ?cycle:Path.joint -> ?scale:(float -> Num.t) ->
      ?color:Color.t -> ?pen:Pen.t -> (float * float) list -> Mlpost.command

val path : 
    ?style:Path.joint -> ?cycle:Path.joint -> ?scale:(float -> Num.t) -> 
      (float * float) list -> Path.t

val jointpath : 
    ?scale:(float -> Num.t) -> (float * float) list -> Path.joint list -> Path.t

val p :
    ?l:Path.direction -> ?r:Path.direction -> 
      ?scale:(float -> Num.t) -> float * float -> Path.knot
