subroutine Create_Shape(xc,yc,Panels,radius,vortx,vorty,h,gamma,mnv)
	!use MAIN_INIT
	implicit none
	integer :: Panels, i, r, dx2, counter, k, pan(2), counter2, mnv
	real :: xc(Panels+1), yc(Panels+1), radius, vortx(mnv), vorty(mnv), h, dx, dist, dy
	real :: xp, yp, pi, gamma(mnv)
	
	pi = 3.14159265359;
	do i=0,Panels
		xc(i+1) = radius*cos(2*pi*(i)/(Panels))
		yc(i+1) = radius*sin(2*pi*(i)/(Panels))
	enddo
	
	dx = 1.1/h;
	dx2 = nint(dx);
	counter = 1;
	do i=1,dx2
		do r=1,dx2
			!print *, counter
			vortx(counter)=-0.55+h*i
			vorty(counter)=-0.55+h*r
			!vortx(counter)=-0.55+h*i
			!vorty(counter)=-0.55+h*r
			gamma(counter)=0
			dist = 10
			counter2 = 1
			dy = 1
			do k=1,Panels
				IF ((vortx(counter)-xc(k))/abs(vortx(counter)-xc(k))/=(vortx(counter)-xc(k+1))/abs(vortx(counter)-xc(k+1))) THEN
					pan(counter2) = k;
					
					counter2 = counter2 + 1;
				END IF
				!dx = (vortx(counter)-(xc(k+1)+xc(k))/2);
				!dy = (vorty(counter)-(yc(k+1)+yc(k))/2);
				!IF (sqrt(dx*dx+dy*dy)<dist) THEN
				!	dist = sqrt(dx*dx+dy*dy);
				!	pan = k;
				!END IF
			enddo
			!print*, counter2
			IF (counter2 == 3) THEN
				dy = (vorty(counter)-yc(pan(1)))*(vorty(counter)-yc(pan(2)));
				!print*, dy
			END IF
			IF (dy > 0) THEN
				counter = counter+1;
			END IF
		enddo
	enddo
	

	!print*, counter
	!print*, dx2
return
end subroutine Create_Shape