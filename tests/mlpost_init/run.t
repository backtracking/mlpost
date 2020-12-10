  $ mlpost-init foo bar

  $ dune build --root=mlpost_figures @mps
  Entering directory 'mlpost_figures'

  $ dune build --root=mlpost_figures @pgf
  Entering directory 'mlpost_figures'

  $ dune build --root=mlpost_figures foo.png bar.png
  Entering directory 'mlpost_figures'

  $ ls mlpost_figures/
  _build
  bar.ml
  bar.mps
  bar.pgf
  bar.png
  common_for_figures.ml
  dune
  dune-project
  foo.ml
  foo.mps
  foo.pgf
  foo.png

  $ cat >>mlpost_figures/common_for_figures.ml <<EOF
  > 
  > let tex = tex ~stroke:(Some Color.blue)
  > EOF

  $ cp mlpost_figures/foo.pgf mlpost_figures/foo.bak.pgf

  $ dune build --root=mlpost_figures @pgf
  Entering directory 'mlpost_figures'

  $ cmp mlpost_figures/foo.bak.pgf mlpost_figures/foo.pgf
  mlpost_figures/foo.bak.pgf mlpost_figures/foo.pgf differ: char 62, line 3
  [1]


#  $ rm -rf mlpost_figures
#
#  $ cat >>main.tex <<EOF
#  > \documentclass{article}
#  > \newcommand{\grand}[1]{\huge{#1}}
#  > \begin{document}
#  > EOF
#
#  $ mlpost-init foo --latex main.tex
#
#  $ cat >>mlpost_figures/common_for_figures.ml <<EOF
#  > 
#  > let tex s = tex (Printf.sprintf "\\\\grand{%s}" s)
#  > EOF
#
#  $ dune build --root=mlpost_figures @pgf
#  Entering directory 'mlpost_figures'
#
#  $ cp mlpost_figures/foo.pgf mlpost_figures/foo.bak.pgf
#
#  $ cat >>main.tex <<EOF
#  > \documentclass{article}
#  > \newcommand{\grand}[1]{\tiny{#1}}
#  > \begin{document}
#  > EOF
#
#  $ dune build --root=mlpost_figures @pgf
#  Entering directory 'mlpost_figures'
#
#  $ cmp -u mlpost_figures/foo.bak.pgf mlpost_figures/foo.pgf
#  mlpost_figures/foo.bak.pgf mlpost_figures/foo.pgf differ: char 62, line 3
#  [1]
