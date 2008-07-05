open Mlpost
open Point
open Command
open Path
open Diag
open Num

let foi, iof = float_of_int, int_of_float

let nodes = 
  Array.init 6
    (fun i -> Array.init 4
       (fun j -> node (foi i) (foi j) ""))
(* 	  (Printf.sprintf "(%d,%d)" i j))) *)

let nodesl = List.flatten (List.map Array.to_list (Array.to_list nodes))
let diag = create nodesl

let add = arrow diag ~head:false
let edges =
  for i = 0 to 5 do
    for j = 0 to 3 do
      (try (add ~outd:(Angle 45.) nodes.(i).(j) nodes.(i+1).(j+1)) with _ -> ());
      (try (add ~outd:(Angle 135.) nodes.(i).(j) nodes.(i-1).(j+1)) with _ -> ());
      (try (add ~outd:Up nodes.(i).(j) nodes.(i).(j+1)) with _ -> ());
    done
  done

let graph = draw ~fill:(Color.gray 0.8) ~style:(Circle (Box.Padding(f 6.,f 6.))) diag
let fig = [draw_pic (Picture.transform [Transform.scaled (f 0.5)] (Picture.make graph))]
