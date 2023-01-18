# MLPost

This is MLPost, an OCaml interface to MetaPost.

Quick links:

* [Usage]
* [Options]
* [Cairo Output]
* [API documentation]
* [Examples]
* [FAQ]

## Installation

`mlpost` can be installed with [opam]:

```shell-session
$ opam install mlpost
```

If you don't have `opam`, you can install it following the [how to install opam] guide.

If you can't or don't want to use `opam`, you can build the package with `dune build -p mlpost @install` but you'll first have to install the dependencies by yourself. You can find the list of dependencies in the [dune-project] file.

## Usage

Open the Mlpost module:

```ocaml
open Mlpost
```

Define your figures in an OCaml file `fig.ml`:

```ocaml
let fig_a = ...
let fig_b = ...
```

Each figure has type `Command.t`.

Add some code to emit Metapost code, as follows:

```ocaml
let () = Metapost.emit "file_a" fig_a
let () = Metapost.emit "file_b" fig_b
```

Then run the `mlpost` program on this file:

```shell-session
$ mlpost fig.ml
```

It will create PostScript figures in files `file_a.mps`, `file_b.mps`, etc.

## Options

`mlpost` supports the following options:

```man
-pdf
	creates .mps files instead of .1, for inclusion in LaTeX files
	compiled with pdflatex (the PostScript file is actually the
	same, but the suffix is used by pdflatex to identify
	PostScript produced by Metapost)

        The bash script view-mps, enclosed in mlpost repo, can be used
        to view .mps files.

-latex main.tex
        indicates the main LaTeX file, from which the prelude is
        extracted to be passed to Metapost (this way you can use
        macros, fonts and packages from your LaTeX document in your
        figures).

-xpdf
        opens an xpdf viewer with the generated figure. Subsequent calls with
        the option -xpdf will refresh the viewer, if it is still open.

-native
        compile to native code. This is usually faster.

-eps
        produce standalone postscript files

-ocamlbuild
        use ocamlbuild to compile the source; this may be useful if there are
        a lot of dependencies

-ccopt <options>
        pass options to the ocaml compiler

-execopt <options>
        pass options to the compiled program
```

## Cairo output

The following functions are not supported in combination with the `Concrete` /
`Cairo` modules:

* `Path.build_cycle`
* `Pen.square`
* `Pen.from_path`

## FAQ

### When I run the `mlpost` tool on my figure, I get the error `! Unable to make mpx file.`.

This is a cryptic error message from MetaPost saying that there is some error in the Latex code that is part of your figure. However, it often points to some random Latex code, so you will have to figure out the problem by yourself, or by looking at the `mpxerr.tex` file that has been generated. You can also try to pass the `mpxerr.tex` file to latex to see which is the exact latex error message.

### When I look at generated `foo.1` or `foo.mps` file, `gv`/`evince` does not display the figure correctly / gives some error.

These generated files are not proper PostScript files. They need to be
included in a Latex file using `\includegraphics`. If you pass the `-eps` option
to `mlpost`, it generates encapsulated PostScript files that can be viewed with
a PostScript viewer like `gv`. However, font rendering may be quite different.

### In my Latex prelude I include other Latex files using `\input{foo.tex}`. When I compile my figure with `mlpost`, these files are not found.

You are probably compiling your figure and your Latex file in different
directories. You can make the file `foo.tex` visible to Latex changing the
environment variable `$TEXINPUTS` to contain the directory where `foo.tex`
lives.

[API documentation]: http://backtracking.github.io/mlpost
[Cairo Output]: #cairo-output
[dune-project]: ./dune-project
[Examples]: http://mlpost.lri.fr/examples
[FAQ]: #faq
[how to install opam]: https://opam.ocaml.org/doc/Install.html
[opam]: https://opam.ocaml.org
[Options]: #options
[Usage]: #usage
