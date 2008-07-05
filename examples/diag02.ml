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
       (fun j -> (node (foi i) (foi j) "", i, j)))
(*  	  (Printf.sprintf "(%d,%d)" i j), i, j))) *)

let nodesl = 
  List.fold_left (fun acc (n,i,j) -> if (i+j) mod 2 = 0 then n::acc else acc)
    [] (List.flatten (List.map Array.to_list (Array.to_list nodes))) 
let diag = create nodesl

let node i j = let (n, _, _) = nodes.(i).(j) in n
let add = arrow diag ~head:false
let edges =
  for i = 0 to 5 do
    for j = 0 to 3 do
      if (i + j) mod 2 = 0 then
	begin
	  (try (add ~outd:(Angle 165.) (node i j) (node (i-3) (j+1))) with _ -> ());
	  (try (add ~outd:(Angle 135.) (node i j) (node (i-1) (j+1))) with _ -> ());
	  (try (add ~outd:(Angle 45.) (node i j) (node (i+1) (j+1))) with _ -> ());
	  (try (add ~outd:(Angle 15.) (node i j) (node (i+3) (j+1))) with _ -> ());
	end
    done
  done

let graph = draw ~fill:(Color.gray 0.8) ~style:(Circle (Num.bp 6.)) diag
let fig = [draw_pic (Picture.transform [Transform.scaled (f 0.5)] (Picture.make graph))]
