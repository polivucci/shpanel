function [y] = BSPG_xc(Npc,xp,yp,grado,Np,xc)
% definisco i punti di controllo
ordine = grado+1;

Npuntipar = 1001;
t = linspace(0,1,Npuntipar);

% trovo i nodi
nodi=knots(Npc,ordine);
% trovo le basi
basi=basis(ordine,Npuntipar,t,Npc,nodi);
% calcolo i valori
xx = basi*xp';
yy =basi*yp';
% interpolazione sui miei valori
y = interp1(xx,yy,xc,'pchip');
