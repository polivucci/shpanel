function [y] = BSPG_xc(Npc,xp,yp,grado,Np,xc)
% control points are defined
ordine = grado+1;

Npuntipar = 1001;
t = linspace(0,1,Npuntipar);

% finding nodes
nodi=knots(Npc,ordine);
% finding basis
basi=basis(ordine,Npuntipar,t,Npc,nodi);
% calculating values
xx = basi*xp';
yy =basi*yp';
% interpolating values
y = interp1(xx,yy,xc,'pchip');
