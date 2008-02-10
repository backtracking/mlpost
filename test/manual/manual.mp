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

beginfig(149)
  def couleur(expr x) =
    if x>.5: (2*(1-x)) [red, blue];
    else:    (1-2x) [blue, red]
    fi
  enddef;
  for i=0 step .5 until 360:
    draw 2cm* (sind(2*i), cosd(3*i))
    % withpen pencircle scaled 2bp
    withcolor couleur(i/360);
  endfor;
endfig;