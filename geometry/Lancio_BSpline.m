%clear
%clc

global Npanels;

%output file is read and translated:
fileIDx = fopen('../shpanel_parameters.dat', 'r');
eingabe = fscanf(fileIDx, '%f %f %f %f %f %f %f', [1 7]);
fclose(fileIDx);

Npanels=eingabe(6);
%Npanels=eingabe(6)
Npunti=(Npanels/2)+1;

%uniform distro
%x_curva = linspace(0,1,Npunti); % generico vettore x di Npunti 

%cosinusoidal distro
x_curva(1)=0;
x = linspace(0,1,Npunti);
x_curva = (1-cos(pi*x))/2;

ll=length(x_curva);
%plot(x_curva,ones(ll),'r+');
grado = 3; % grado della curva BSpline

% coordinate dei vertici del poligono di controllo

%camber Dang
%x1 = 0;
%x2 = 0.10;%x2 = 0.128;
%x3 = 0.42;%x3 = 0.463;
%x4 = 0.772;
%x5 = 0.903;
%x6 = 1;

%y1 = 0;
%y2 = 0.00568;
%y3 = 0.45;%y3 = 0.448;
%y5 = 0.25;%y5 = 0.283;
%y6 = 0;

%camber a08mod
x1 = 0;
x2 = 0.045;
x3 = 0.279;
x4 = 0.643;
x5 = 0.801;
x6 = 1;

y1 = 0;
y2 = 0.350659299353481;
y3 = 0.989293339347467;
y4 = 1.043;
y5 = 0.781601112614644;
y6 = 0;


%camber a08
%x1 = 0;
%x2 = 0.038;
%x3 = 0.297;
%x4 = 0.738;
%x5 = 0.812;
%x6 = 1;

%y1 = 0;
%y2 = 0.321684830633284;
%y3 = 1.08226362297495;
%y4 = 0.973999999999995;
%y5 = 0.676650957290133;
%y6 = 0;

%thickess NACA66
x1t = 0;
x2t = 0;
x3t = 0.213;
x4t = 0.576;
x5t = 0.916;
x6t = 1;

y1t = 0;
y2t = 0.198533696090292;
y3t = 0.464263280935097;
y4t = 0.575200000000007;
y5t = 0.225181700927042;
y6t = 0;

X = [x1 x2 x3 x4 x5 x6];
Y = [y1 y2 y3 y4 y5 y6];

Xt = [x1t x2t x3t x4t x5t x6t];
Yt = [y1t y2t y3t y4t y5t y6t];


Npoligono = size(X,2);  %numero di colonne (dimensione) del vettore che contiene le coordinate punti di controllo
Npoligonot = size(Xt,2);

[y_curva]=BSPG_xc(Npoligono,X,Y,grado,Npunti,x_curva);
[t_curva]=BSPG_xc(Npoligonot,Xt,Yt,grado,Npunti,x_curva);
%[t_curva_temp]=BSPG_xc(Npoligonot,Xt,Yt,grado,Npunti,x_curva);
%GR=max(t_curva_temp);
%[t_curva]=t_curva_temp/(2*GR);

%plot(x_curva,y_curva,X,Y);
%hold on
%postprocessing

fileIDs = fopen('../foil/spline_line.dat', 'w');
%fprintf(fileIDs, '%6s %6s\n', '#x', 'y');
fprintf(fileIDs, '%24.20f %24.20f\n', [x_curva; y_curva]);
fclose(fileIDs);

fileIDss = fopen('../foil/spline_thick.dat', 'w');
%fprintf(fileIDs, '%6s %6s\n', '#x', 'y');
fprintf(fileIDss, '%24.20f %24.20f\n', [x_curva; t_curva]);
fclose(fileIDss);

%APPLYING DIMENSIONALIZATION============================
%c=1;
%fmax=0.1;
%tmax=0.1;

%x_curva=x_curva.*c;
%y_curva=y_curva.*fmax;
%t_curva=t_curva.*tmax;

%DISCRETIZATION=========================================

%%Erstellung des gerichteten Koordinatenvektors
%xinv=fliplr(x_curva);
%yinv=fliplr(y_curva);
%tinv=fliplr(t_curva);
%xo=[xinv x_curva(2:ll)];

%%Berechnung der Mittelkurveableitung
%for lin=2:(ll-1)
%delta2(lin) = abs(x_curva(lin+1) - x_curva(lin-1));
%ja(lin) = atan((-y_curva(lin+1) + y_curva(lin-1))./delta2(lin));
%end

%ex(1)=xo(1);
%ypsilon(1)=0;

%%THICKNESS NORMAL TO CL
%ex(2:Npunti) = xinv(2:Npunti) + tinv(2:Npunti).*sin(ja);
%ypsilon(2:Npunti) = yinv(2:Npunti) - tinv(2:Npunti).*cos(ja);

%ex(Npunti+1:Npanels+1) = x_curva(2:Npunti) - t_curva(2:Npunti).*sin(-ja);
%ypsilon(Npunti+1:Npanels+1) = y_curva(2:Npunti) + t_curva(2:Npunti).*cos(-ja);

%ex(Npanels+1)=ex(1);
%ypsilon(Npanels+1)=ypsilon(1);

%plot(ex, ypsilon, 'r');

%fileIDsss = fopen('spline_discr.dat', 'w');
%%fprintf(fileIDsss, '%6s %6s\n', '#x', 'y');
%fprintf(fileIDsss, '%6.12f %6.12f\n', [ex; ypsilon]);
%fclose(fileIDsss);
