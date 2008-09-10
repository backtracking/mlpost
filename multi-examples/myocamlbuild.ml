open Ocamlbuild_plugin
open Command
open Pathname

let find_files base =
  let lbase = String.length base in
  let files = Array.to_list (readdir ".") in
  let files = List.filter begin fun f ->
    check_extension f "1" && String.sub f 0 lbase = base
  end files in
  List.sort String.compare files

let shell x = Printf.ksprintf (fun s -> Cmd(Sh s)) x
let line f x = Printf.ksprintf (fun s -> Cmd(Sh("echo \""^s^"\" >> "^f))) x

let () = dispatch begin function
  | After_rules ->
      rule ".byte -> .htmlpart"
        ~prod: "%.htmlpart"
        ~deps: ["%.byte"; "all.template"]
        begin fun env build ->
          (* Execute the program *)
          Command.execute (Cmd(S[A(env "./%.byte"); Sh "> /dev/null"]));
          (* DELETE "> /dev/null" IF YOU WANT TO DEBUG *)
          (* List its .1 files *)
          let files = find_files (env "%-") in
          let files = List.map Filename.chop_extension files in
          let files = List.map (fun f -> f ^ ".png") files in
          (* Compile .png files *)
          List.iter Outcome.ignore_good (build (List.map (fun x -> [x]) files));
          (* Produce the .htmlpart file *)
          let f = env "%.htmlpart" in
          Seq [
            shell "echo > %s" f;
            line f "<h2>%s:</h2>" (env "%.ml");
            Seq (List.map (fun png -> line f "<img src=\"%s\">" png) files);
          ]
        end;

      rule ".byte -> .1"
        ~prod: "%(base)-%(num).1"
        ~dep: "%(base).byte"
        begin fun env _ ->
          Cmd(A(env "./%(base).byte"))
        end;

      rule ".1 -> .dvi"
        ~prod: "%.dvi"
(*        ~dep: "%.1" (* IGNORE THIS TO MAKE THE .htmlpart RULE WORK *) *)
        begin fun env _ ->
          let base = env "%" in
          let sed =
            Sh(Printf.sprintf "sed -e 's/all/%s/' all.template > %s.tex"
                 base base)
          in
          let latex = S[
            A "latex"; A "-interaction"; A "nonstopmode";
            A "-file-line-error"; A "-halt-on-error";
            A base;
            Sh "> /dev/null"; (* DELETE THIS IF YOU WANT TO DEBUG *)
          ] in
          Seq[Cmd sed; Cmd latex]
        end;

      rule ".dvi -> .ps"
        ~prod: "%.ps"
        ~dep: "%.dvi"
        begin fun env _ ->
          Cmd(S[A "dvips"; A "-q"; A "-E"; A(env "%.dvi"); A "-o"])
        end;

      rule ".ps -> png"
        ~prod: "%.png"
        ~dep: "%.ps"
        begin fun env _ ->
          Cmd(S[A "convert"; A(env "%.ps"); A(env "%.png")])
        end;

      flag ["ocaml"; "compile"] (S [A "-I";A "../../"; A "mlpost.cma"]);
      flag ["ocaml"; "link"] (A "../../mlpost.cma")
  | _ -> ()
end
