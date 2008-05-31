beginfig(1);
draw (20,20)--(0,0)--(0,30)--(30,0)--(0,0);
endfig;

beginfig(3);
z0 = (0,0);    z1 = (60,40);
z2 = (40,90);  z3 = (10,70);
z4 = (30,50);
draw z0..z1..z2..z3..z4;
dotlabels.top(0,2,4);
dotlabels.lft(3);
dotlabels.lrt(1);
endfig;

beginfig(104);
z0 = (0,0);    z1 = (60,40);
z2 = (40,90);  z3 = (10,70);
z4 = (30,50);
draw z0..z1..z2..z3..z4..cycle;
dotlabels.top(2,4);
dotlabels.lft(0,3);
dotlabels.lrt(1);
endfig;

beginfig(204);
z0 = (0,0);    z1 = (60,40);
z2 = (40,90);  z3 = (10,70);
z4 = (30,50);
draw z0..z1..z2..z3--z4--cycle;
dotlabels.top(2,4);
dotlabels.lft(0,3);
dotlabels.lrt(1);
endfig;

beginfig(5);
z0 = (0,0);    z1 = (60,40);
z2 = (40,90);  z3 = (10,70);
z4 = (30,50);
path p; p = z0..z1..z2..z3..z4;
draw p;
for t=0 upto 3:
  draw point t of p--postcontrol t of p
  --precontrol t+1 of p--point t+1 of p
  withcolor 0.5white
  %dashed (evenly scaled .5)
    ;
endfor
dotlabels.top(0,2,4);
dotlabels.lft(3);
dotlabels.lrt(1);
endfig;

beginfig(6);
z0 = (0,0);    z1 = (60,40);
z2 = (40,90);  z3 = (10,70);
z4 = (30,50);
draw z0..z1{up}..z2{left}..z3..z4;
dotlabels.top(0,2,4);
dotlabels.lft(3);
dotlabels.lrt(1);
endfig;

beginfig(7)
for a=0 upto 9:
  draw (0,0){dir 45}..{dir -10a}(6cm,0);
endfor
endfig;

beginfig(8)
for a=0 upto 7:
  draw (0,0){dir 45}..{dir 10a}(6cm,0);
endfor
endfig;

beginfig(109);
z2=-z0=(1in,0); z1=(0,.2in);
draw z0{up}..z1{right}..z2{down};
dotlabels.bot(0,1,2);
endfig;

beginfig(209);
z2=-z0=(1in,0); z1=(0,.2in);
draw z0{up}...z1{right}...z2{down};
dotlabels.bot(0,1,2);
endfig;

beginfig(110);
numeric u; 10u=1.5in;
-z0=z3=(5u,0);
(-x1,y1)=z2=(3u,2u);
draw z0..z1..z2..z3;
dotlabels.bot(0,1,2,3);
endfig;

beginfig(210);
numeric u; 10u=1.5in;
-z0=z3=(5u,0);
(-x1,y1)=z2=(3u,2u);
draw z0..z1..tension 1.3..z2..z3;
dotlabels.bot(0,1,2,3);
endfig;

beginfig(310);
numeric u; 10u=1.5in;
-z0=z3=(5u,0);
(-x1,y1)=z2=(3u,2u);
draw z0..z1..tension 1.5 and 1..z2..z3;
dotlabels.bot(0,1,2,3);
endfig;

beginfig(111);
numeric u, c; 10u=1.4in; c=0;
z1=(0,0); (x0,-y0)=z2=(2u,5u);
draw z0{curl c}..z1..{curl c}z2;
dotlabels.rt(0,1,2);
endfig;

beginfig(211);
numeric u, c; 10u=1.4in; c=1;
z1=(0,0); (x0,-y0)=z2=(2u,5u);
draw z0{curl c}..z1..{curl c}z2;
dotlabels.rt(0,1,2);
endfig;

beginfig(311);
numeric u, c; 10u=1.4in; c=2;
z1=(0,0); (x0,-y0)=z2=(2u,5u);
draw z0{curl c}..z1..{curl c}z2;
dotlabels.rt(0,1,2);
endfig;

beginfig(411);
numeric u, c; 10u=1.4in; c=infinity;
z1=(0,0); (x0,-y0)=z2=(2u,5u);
draw z0{curl c}..z1..{curl c}z2;
dotlabels.rt(0,1,2);
endfig;

beginfig(17) ;
a=.7in ; b=.5in ;
z0=(0,0) ;
z1=-z3=(a,0) ;
z2=-z4=(0,b) ;
draw z1..z2..z3..z4..cycle ;
draw z1 -- z0 -- z2 ;
label.top("a",.5[z0,z1]) ;
label.lft("b",.5[z0,z2]) ;
dotlabel.bot("(0,0)",z0) ;
endfig ;

beginfig(18) ;
  numeric u;
  u = 1cm;
  draw (0,2u) -- (0,0) -- (4u,0) ;
  pickup pencircle scaled 1pt ;
  draw (0,0){up}
  for i=1 upto 8 :
    ..(i/2,sqrt(i/2))*u endfor ;
  label.lrt(btex $ \sqrt x$ etex,
    (3,sqrt 3)*u) ;
  label.bot(btex $x$ etex, (2u,0)) ;
  label.lft(btex $y$ etex, (0,u)) ;
endfig ;

beginfig(19) ;
  numeric ux, uy ;
  120ux=1.2in; 4uy=2.4in ;
  draw (0,4uy) -- (0,0) -- (120ux,0) ;
  pickup pencircle scaled 1pt ;
  draw (0,uy){right}
  for ix=1 upto 8 :
    ..(15ix*ux, uy*2/(1+cosd 15ix))
  endfor ;
  label.bot (btex axe $x$ etex, (60ux,0)) ;
  label.lft (btex axe $y$ etex
    rotated 90, (0,2uy)) ;
  label.lft (btex
    $\displaystyle y={2\over1+\cos x}$ etex,
    (120ux, 4uy)) ;
endfig ;

beginfig(21) ;
path p ;
p = (-1cm,0)..(0,-1cm)..(1cm,0) ;
fill p{up} .. (0,0){-1,-2} ..
{up}cycle ;
draw p .. (0,1cm) .. cycle ;
endfig ;

beginfig(22) ;
path a, b, aa, ab ;
a = fullcircle scaled 2cm ;
b = a shifted (0,1cm) ;
aa = halfcircle scaled 2cm ;
ab = buildcycle(aa, b) ;
picture pa, pb ;
pa = thelabel(btex $A$ etex, (0,-
.5cm)) ;
pb = thelabel(btex $B$ etex,
(0,1.5cm)) ;
fill a withcolor .7white ;
fill b withcolor .7white ;
fill ab withcolor .4white ;
unfill bbox pa ;
draw pa ;
unfill bbox pb ;
draw pb ;
label.lft(btex $U$ etex, (-
1cm,.5cm)) ;
draw bbox currentpicture ;
endfig ;

beginfig(40);
path p[];
p1 = (0,0){curl 0}..(5,-3)..{curl 0}(10,0);
p2 = p1..(p1 yscaled-1 shifted(10,0));
p0 = p2;
for i=1 upto 3:  p0:=p0.. p2 shifted (i*20pt,0);
  endfor
for j=0 upto 8:  draw p0 shifted (0,j*10pt);
  endfor
p3 = fullcircle shifted (.5,.5) scaled 72pt;
clip currentpicture to p3;
draw p3;
endfig;

