  val draw_spline : Cairo.t -> Spline_lib.Epure.t -> unit
  val draw_tex : Cairo.t -> Gentex.t -> unit

module MetaPath :
  sig
    type pen = Matrix.t
    val stroke : Cairo.t -> pen -> Spline_lib.path -> unit
    val fill : Cairo.t -> Spline_lib.path -> unit
    val draw_path : Cairo.t -> Spline_lib.path -> unit
  end

module Picture :
sig
  val draw : Cairo.t -> float -> float -> Picture_lib.t -> unit
  val where : Cairo.t -> Picture_lib.t -> float * float -> Picture_lib.id list
  val move : 
    Cairo.t -> Picture_lib.t -> Picture_lib.id -> float * float -> float * float
end

