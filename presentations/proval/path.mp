verbatimtex
%&latex
\documentclass[nodefaultblocks]{beamer}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{beamerthemesplit}
\usepackage{stmaryrd}
\usepackage{amsfonts}
\usepackage{latexsym}
\usepackage{amssymb}
\usepackage{amsmath}
\usepackage{mathrsfs}
\usepackage{pifont}
\usepackage{shadow}   
\usepackage{alltt}   
\usepackage[dvips]{epsfig}

\usetheme{Madrid}

\beamertemplatenavigationsymbolsempty

\definecolor{yel}{rgb}{.9,.8,.4}
\definecolor{mygray}{rgb}{0.85,0.85,0.85}

\def \ebox #1{\colorbox{yel}{\mbox {\strut #1}}}
\def \gbox #1{\colorbox{mygray}{\mbox {\strut #1}}}

\title{MlPost}
\author{The MlPost team}
\institute{Proval}
\date{Réunion Proval}

\begin{document}
etex
input boxes;
beginfig(1)
  draw (0.0000,0.0000) ..controls (26.8000,-1.8000) and (51.4000,14.6000) ..
    (60.0000,40.0000) ..controls (67.1000,61.0000) and (59.8000,84.6000) ..
    (40.0000,90.0000) ..controls (25.4000,94.0000) and (10.5000,84.5000) ..
    (10.0000,70.0000) ..controls (9.6000,58.8000) and (18.8000,49.6000) ..
    (30.0000,50.0000);
  draw (0.0000,0.0000) -- (26.8000,-1.8000) -- (51.4000,14.6000) --
    (60.0000,40.0000) -- (67.1000,61.0000) -- (59.8000,84.6000) --
    (40.0000,90.0000) -- (25.4000,94.0000) -- (10.5000,84.5000) --
    (10.0000,70.0000) -- (9.6000,58.8000) -- (18.8000,49.6000) --
    (30.0000,50.0000) dashed evenly scaled 0.5000 ;
  dotlabel.top(btex 0 etex, (0.0000,0.0000));
  dotlabel.top(btex 2 etex, (40.0000,90.0000));
  dotlabel.top(btex 4 etex, (30.0000,50.0000));
  dotlabel.lft(btex 3 etex, (10.0000,70.0000));
  dotlabel.rt(btex 1 etex, (60.0000,40.0000));
   endfig;
