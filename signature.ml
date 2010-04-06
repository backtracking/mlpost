type point = Types.point
type num = Types.num

module type Boxlike =
sig
  type t
  val width : t -> num
  val height : t -> num
  val set_pos : point -> t -> t
end
