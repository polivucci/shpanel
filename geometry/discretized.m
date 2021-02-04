clear
clc

%THICKNESS NORMAL TO CL
IF (xs(inx) < xs(inxold)) THEN
ja = ATAN((-ys(inx+1) + ys(inx-1))/delta2)
x(k) = xs(inx) + ts(inx)*SIN(ja)
y(k) = ys(inx) - ts(inx)*COS(ja)

ELSE
ja = ATAN((ys(inx+1) - ys(inx-1))/delta2)
x(k) = xs(inx) - ts(inx)*SIN(ja)
y(k) = ys(inx) + ts(inx)*COS(ja)

ENDIF

fileIDsss = fopen('spline_discr.dat', 'w');
%fprintf(fileIDsss, '%6s %6s\n', '#x', 'y');
fprintf(fileIDsss, '%6.4f %6.4f\n', [x_curva; t_curva]);
fclose(fileIDsss);
