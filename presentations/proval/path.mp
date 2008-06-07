beginfig(1);
z0 = (0,0);    z1 = (60,40);
z2 = (40,90);  z3 = (10,70);
z4 = (30,50);
path p; p = z0..z1..z2..z3..z4;
draw p;
for t=0 upto 3:
  draw point t of p--postcontrol t of p
  --precontrol t+1 of p--point t+1 of p
  %withcolor 0.5white
  dashed (evenly scaled .5)
    ;
endfor
dotlabels.top(0,2,4);
dotlabels.lft(3);
dotlabels.lrt(1);
endfig;