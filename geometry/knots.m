function[ku] = knots(nup,p)
nku = nup + p;
ku(1:p) = 0;
c = nup-p;
for i=1:c
ku(i+p) = i/(c+1);
end
ku(c+p+1:nku) = 1;