function[n] = basis(c,nt,t,npts,x)
nplusc = npts+c;

	for it=1:nt													%for each t in t[]	
%		calculate the first order basis functions n[i][1]
		temp = 0;		 
		for i=1:nplusc-1
			if ((t(it) >= x(i))&&(t(it) < x(i+1)))
				temp(i) = 1;
			else
				temp(i) = 0;
            end
        end

%		calculate the higher order basis functions
		for k=2:c 
			for i=1:nplusc-k
				if ((temp(i) ~= 0))                  %if the lower order basis function is zero skip the calculation
					d = ((t(it)-x(i))*temp(i))/(x(i+k-1)-x(i));
				else
					d = 0;
                end
			
				if ((temp(i+1) ~= 0)) 				%if the lower order basis function is zero skip the calculation
					e = ((x(i+k)-t(it))*temp(i+1))/(x(i+k)-x(i+1));
				else
					e = 0;
                end

				temp(i) = d + e;
            end
        end

%		pick up last point	
		if (t(it) == x(nplusc))
			temp(npts) = 1;
        end

%	put in n array
		for i=1:npts 
			n(it,i) = temp(i);
        end
    end	

end