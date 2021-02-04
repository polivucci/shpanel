%clear
%clc

global Npanels;

% read in flow parameters:
fileIDx = fopen('../shpanel_parameters.dat', 'r');
eingabe = textscan(fileIDx, '%f', 'CommentStyle', '#'){1};
fclose(fileIDx);

% read in geometry:
fileIDx = fopen('../geometry_input.dat', 'r');
geometry_input = textscan(fileIDx, '%f %f %f %f', 'CommentStyle', '#');
X= geometry_input{1}'; Y= geometry_input{2}';
Xt=geometry_input{3}'; Yt=geometry_input{4}';
fclose(fileIDx);

Npanels=eingabe(6);
Npunti=(Npanels/2)+1;

%uniform discretization:
%x_curva = linspace(0,1,Npunti); % generico vettore x di Npunti 

%cosinusoidal graded distribution:
x_curva(1)=0;
x = linspace(0,1,Npunti);
x_curva = (1-cos(pi*x))/2;

ll=length(x_curva);
grado = 3; % BSpline curve degree

%dimension of control point vectors:
Npoligono = size(X,2);
Npoligonot = size(Xt,2);

[y_curva]=BSPG_xc(Npoligono,X,Y,grado,Npunti,x_curva);
[t_curva]=BSPG_xc(Npoligonot,Xt,Yt,grado,Npunti,x_curva);

fileIDs = fopen('./spline_line.dat', 'w');
%fprintf(fileIDs, '%6s %6s\n', '#x', 'y');
fprintf(fileIDs, '%24.20f %24.20f\n', [x_curva; y_curva]);
fclose(fileIDs);

fileIDss = fopen('./spline_thick.dat', 'w');
%fprintf(fileIDs, '%6s %6s\n', '#x', 'y');
fprintf(fileIDss, '%24.20f %24.20f\n', [x_curva; t_curva]);
fclose(fileIDss);