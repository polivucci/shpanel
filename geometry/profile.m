%builds the discretized profile

clear
clc

global Npunti
global x_curva
global y_curva

%only odd numbers (even number of panels)
Npunti=51;

%sinusoidal distro
x_curva(1)=0;
x = linspace(0,1,((Npunti-1)/2+1));
x_curva = asin(x)/pi;
x_curva((((Npunti-1)/2)+2):Npunti)=1-fliplr(x_curva(1:( (Npunti-1)/2  ) ) );

[y_curva, t_curva] = Lancio_BSpline(x_curva, Npunti)

fileIDs = fopen('spline_profile.dat', 'w');
fprintf(fileIDs, '%6s %6s\n', '#x', 'y');
fprintf(fileIDs, '%6.4f %6.4f %6.4f\n', [x_curva; y_curva; t_curva]);
fclose(fileIDs);
