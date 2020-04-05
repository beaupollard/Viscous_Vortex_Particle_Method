subroutine Panel_Influence(xc,yc,Panels,Bt,Bn)
	implicit none
	integer :: Panels, i, k
	real :: xc(Panels+1), yc(Panels+1), xp, yp, xp2, yp2, num, den, Vng, Vtg, pi, Vns, Vts
	real :: midx, midy, midxI, midyI, dist, csf, ssf, csi, ssi, dx, dy, Vx, Vy, Vxn, Vyn
	real, dimension(Panels+1,Panels) :: Bt
    real, dimension(Panels,Panels) :: Bn
	!real :: vortxp(nv), vortyp(nv), Uinf(2), gamma(nv), sigma, ux0(nv), uy0(nv)
	!real :: ux, uy, q, pi, xp(nv), yp(nv), uxn(nv), uyn(nv)

	pi = 3.14159265359;

	do i=1,Panels
		dx = xc(i+1)-xc(i)
		dy = yc(i+1)-yc(i)
		dist = sqrt(dx*dx+dy*dy)
		csf = dx/dist
		ssf = dy/dist
		do k=1,Panels
            dx = xc(k+1)-xc(k)
            dy = yc(k+1)-yc(k)
			csi = dx/sqrt(dx*dx+dy*dy)
	        ssi = dy/sqrt(dx*dx+dy*dy)
		    midx = (xc(k+1)+xc(k))/2.
	    	midy = (yc(k+1)+yc(k))/2.
            xp = (midx-xc(i))*csf + (midy-yc(i))*ssf
            yp = (midx-xc(i))*ssf - (midy-yc(i))*csf

            
            Vng = -1/(4.*pi)*LOG(((xp-dist)**2.+yp**2)/(xp**2.+yp**2.))
            Vtg = 1/(2.*pi)*atan2(-yp*dist,yp**2.+xp**2.-dist*xp)
            Vns = -Vtg
            Vts = Vng
       
            if (i == k) then
                Vtg = 0.5
                Vng = 0.
                Vns = 0.5 
                Vts = 0.
            end if
         
            Vx = Vng*ssf+Vtg*csf
            Vy = -Vng*csf+Vtg*ssf
            Vxn = Vns*ssf+Vts*csf
            Vyn = -Vns*csf+Vts*ssf
            Bt(k,i) = Vx*csi + Vy*ssi
            Bn(k,i) = Vxn*ssi - Vyn*csi
        end do
        Bt(k,i) = dist
    end do
    
            
 !!           
	!do i=1,Panels
	!	dx = xc(i+1)-xc(i);
	!	dy = yc(i+1)-yc(i);
	!	dist = sqrt(dx*dx+dy*dy);
	!	csf = dx/dist;
	!	ssf = dy/dist;
	!	midx = (xc(i+1)+xc(i))/2;
	!	midy = (yc(i+1)+yc(i))/2;
	!	do k=1,Panels
	!		dx = xc(k+1)-xc(k);
	!		dy = yc(k+1)-yc(k);
	!		csi = dx/sqrt(dx*dx+dy*dy);
	!		ssi = dy/sqrt(dx*dx+dy*dy);
	!		xp = csf*(xc(k)-midx)+ssf*(yc(k)-midy);
	!		yp = -ssf*(xc(k)-midx)+csf*(yc(k)-midy);
	!		xp2 = csf*(xc(k+1)-midx)+ssf*(yc(k+1)-midy);
	!		yp2 = -ssf*(xc(k+1)-midx)+csf*(yc(k+1)-midy);
	!		midxI = (xp2+xp)/2;
	!		midyI = (yp2+yp)/2;
	!		
	!		num = (midxI+dist/2)*(midxI+dist/2)+midyI*midyI;
	!		den = (midxI-dist/2)*(midxI-dist/2)+midyI*midyI;
	!		Vng = 1/(4*pi)*LOG(num/den);
	!		
	!		num = midyI*dist;
	!		den = (midyI*midyI + midxI*midxI - dist*dist/4);
	!		Vtg = -1/(2*pi)*atan2(num,den);
	!		
	!		IF (k == i) THEN
	!			Vng = 0;
	!			Vtg = 0.5;
 !           END IF
	!		
	!		Vx = Vtg*csf - Vng*ssf;
	!		Vy = Vtg*ssf + Vng*csf;
	!		Bt(k,i) = -(Vx*csi + Vy*ssi);
 !           Bn(k,i) = -(Vx*ssi - Vy*csi)
 !           !Vx = Vng*ssf+Vtg*csf
 !           !Vy = -Vng*csf+Vtg*ssf
 !           !Bt(k,i) = Vx*csi + Vy*ssi            
	!		
 !       enddo
 !       Bt(k,i) = dist;
	!enddo
	

return
end subroutine Panel_Influence