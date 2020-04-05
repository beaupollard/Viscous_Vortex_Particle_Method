subroutine Affects_Vorts_Pot(vortx,vorty,gamma,nv,xc,yc,Panels,Vt,Uinf,sgam,Vn,ddthetab,dthetab,vt_ie,flag)
	implicit none
	integer :: Panels, i, k, nv, j, k2, flag
	real :: xc(Panels+1), yc(Panels+1), pi, vortx(nv), vorty(nv), gamma(nv), midx, midy, yp3
	real :: dist, csf, ssf, dx, dy, Vx, Vy, Vt(Panels), sgam, distL, Vtp, phi(2), dx2, dy2, xp2, yp2, Vtt
	real :: Uinf(2), Vn(Panels), ddthetab, dthetab, vt_ie, xp, yp, ti(5), check, phi1, phi2, theta1, theta2, vv3(Panels)
    
	!wi(1) = (18.0-sqrt(30.0))/36.0
	!wi(2) = (18.0+sqrt(30.0))/36.0
	!wi(3) = (18.0+sqrt(30.0))/36.0
	!wi(4) = (18.0-sqrt(30.0))/36.0
	ti(1) = -1./3.*sqrt(5.+2.*sqrt(10./7.))
	ti(2) = -1./3.*sqrt(5.-2.*sqrt(10./7.))
	ti(3) = 0.
	ti(4) = 1./3.*sqrt(5.-2.*sqrt(10./7.))
    ti(5) = 1./3.*sqrt(5.+2.*sqrt(10./7.))

	pi = 3.14159265359
	!print *, Uinf(1), Uinf(2)
    Vt = 0.

    !$OMP PARALLEL DO PRIVATE(dx,dy,distL,csf,ssf,Vx,Vy,xp,yp,xp2,yp2,yp3,theta1,theta2,Vtt)
	do i=1,Panels
        dx = xc(i+1)-xc(i)
		dy = yc(i+1)-yc(i)
		distL = sqrt(dx*dx+dy*dy)
		csf = dx/distL
		ssf = dy/distL
        
		Vx = 0.
		Vy = 0.
        Vtp = 0.
        Vtt = 0.
        do j=1,4
            phi1 = 0.
            phi2 = 0.
            xp = (xc(i+1)+xc(i))/2. + ti(j)*distL*csf/2.
            yp = (yc(i+1)+yc(i))/2. + ti(j)*distL*ssf/2.
            xp2 = (xc(i+1)+xc(i))/2. + ti(j+1)*distL*csf/2.
            yp2 = (yc(i+1)+yc(i))/2. + ti(j+1)*distL*ssf/2.

		    do k=1,nv
                
                dx = -(vortx(k)-xp)
                dy = -(vorty(k)-yp)
                dx2 = -(vortx(k)-xp2)
                dy2 = -(vorty(k)-yp2)

                yp3 = (vortx(k)-(xp+xp2)/2.)*ssf - (vorty(k)-(yp+yp2)/2.)*csf
                if (yp3 < 0.) then
                    dx = -dx
                    dy = -dy
                    dx2 = -dx2
                    dy2 = -dy2
                    theta1 = (atan2(dy,dx))
                    theta2 = (atan2(dy2,dx2))
                else
                    theta1 = (atan2(dy,dx))
                    theta2 = (atan2(dy2,dx2))
                end if
                if (theta1 < -pi/2. .AND. theta2 > 0.) then
                    theta1 = theta1+2.*pi
                end if
                if (theta2 < -pi/2. .AND. theta1 > 0.) then
                    theta2 = theta2+2.*pi
                end if

                Vtt  = Vtt + gamma(k)/(2.*pi)*(theta2-theta1)/sqrt((xp-xp2)**2.+(yp-yp2)**2.)

            enddo
        end do
         vv3(i) = Vtt/4.
        
  
		Vx = Uinf(1) + (yc(i+1)+yc(i))/2*dthetab
		Vy = Uinf(2) - (xc(i+1)+xc(i))/2*dthetab
		!print *, Vx, Vy
		!dx = xc(i+1)-xc(i)
		!dy = yc(i+1)-yc(i)
		!!print *, dx, dy
		!dist = sqrt(dx*dx+dy*dy)
		!csf = dx/dist;
		!ssf = dy/dist;
        !if (xc(i+1)==xc(i)) then
        !    csf = 0.
        !    ssf = 1.
        !elseif (yc(i+1)==yc(i)) then
        !    csf = 1.
        !    ssf = 0.
        !end if
        !if (i==499) then
        !    print *, "Vx:", Vx, "csf:", csf, "ssf", ssf, "Vy:", Vy, "Vtt:", Vtt
        !end if

		Vt(i) = -(Vx*csf + Vy*ssf + Vtt/4.)
        !if (isnan(Vt(i))) then
        !    print *, Vx, Vy, csf, ssf
        !end if
        
        Vn(i) = (Vx*ssf - Vy*csf)
    enddo
    !$OMP END PARALLEL DO
    !call Print_Data(vv3,Panels,1,"D:/bpoll/Fortran/Large_Data/Foil/vv3.dat",40,61,1)
    
    
	dx = 0.
	do i=1,nv
		dx = dx + gamma(i)
    enddo
    !dx = 0.
	vt_ie = -(dx+sgam+ddthetab)
	return
end subroutine Affects_Vorts_Pot