open Mlpost
open Command

(*parse <<togglescript>> *)

(*parse <<include1 *)

let include1 =
  externalimage "powered-by-caml.128x58.png" 
    (`Width (Num.cm 3.))

(*parse >> *)

(*parse <<include2 *)

let include2 =
  externalimage "powered-by-caml.128x58.png" 
    (`Inside (Num.cm 3.,Num.cm 3.))

(*parse >> *)


let () = List.iter (fun (i,fig) -> 
                      Metapost.emit ("include"^(string_of_int i)) fig)
  [1,include1;
   2,include2]
