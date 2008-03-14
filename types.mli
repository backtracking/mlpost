
type num = float

type name = string

type corner = N | S | W | E | NE | NW | SW | SE

type point = 
  | PTPair of num * num
  | PTBoxCorner of name * corner
  | PTPointOf of float * path
  | PTAdd of point * point
  | PTSub of point * point
  | PTMult of float * point
  | PTRotated of float * point

and path =
  unit

and transform =
  | TRRotated of float
  | TRScaled of num
  | TRShifted of point
  | TRSlanted of num
  | TRXscaled of num
  | TRYscaled of num
  | TRZscaled of point
  | TRReflect of point * point
  | TRRotateAround of point * float

and picture = 
  | PITex of string
