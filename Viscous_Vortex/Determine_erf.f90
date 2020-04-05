subroutine Determine_erf(xc,yc,Panels,vortx,vorty,h,gamma,nv,erf,Vt,nu,Dt)
	implicit none
	integer :: i, j, Panels, nv, pan(nv), k, b, ind, counter, j2
	real :: vortx(nv), vorty(nv), gamma(nv), xc(Panels+1), yc(Panels+1), dist, dx, dy, h
	real, dimension(100,2) :: erf
	real :: midx, midy, xc2, yc2, csf(Panels), ssf(Panels), del(Panels), sig, pi
	real :: Vt(Panels+1), nu, Dt, wi(4), p(6), hi2, hi, delT, ti(4), erfc(6), ierfc(4), DG
	real :: DG2(nv), DG4, DG3(nv), check(Panels), DG5, DGN
	
	!wi(1) = (/ (18-(30)**(0.5))/36, (18+(30)**(0.5))/36, (18+(30)**(0.5))/36, (18-(30)**(0.5))/36 /)

	wi(1) = (18.0-sqrt(30.0))/36.0
	wi(2) = (18.0+sqrt(30.0))/36.0
	wi(3) = (18.0+sqrt(30.0))/36.0
	wi(4) = (18.0-sqrt(30.0))/36.0
	ti(1) = -sqrt(3.0/7.0+2.0/7.0*sqrt(6.0/5.0))
	ti(2) = -sqrt(3.0/7.0-2.0/7.0*sqrt(6.0/5.0))
	ti(3) = sqrt(3.0/7.0-2.0/7.0*sqrt(6.0/5.0))
	ti(4) = sqrt(3.0/7.0+2.0/7.0*sqrt(6.0/5.0))
	pi = 3.14159265359;

	do i=1,Panels
		dist = sqrt((xc(i+1)-xc(i))**2+(yc(i+1)-yc(i))**2)
		del(i) = dist
		csf(i) = (xc(i+1)-xc(i))/dist
		ssf(i) = (yc(i+1)-yc(i))/dist
	enddo
	do i=1,nv
		DG2(i) = 0
	enddo
	do i=1,Panels
		midx = (xc(i+1)+xc(i))/2
		midy = (yc(i+1)+yc(i))/2
		dist = 100
		counter = 1
		DG4 = 0
		DG5 = 0
		do j2=1,nv
			!print *, j2
			!print *, nv
			
			dx = vortx(j2)-midx
			dy = vorty(j2)-midy
			dist = sqrt(dx*dx+dy*dy)
			if (dist<h*10.) then
				pan(counter) = j2
				!print *, pan(counter)
				xc2 = ssf(i)*dx-csf(i)*dy
				yc2 = csf(i)*dx+ssf(i)*dy
				!print *, 'flag1'
				if (xc2 < 3/2*h) then
					hi2 = h/2
					hi = xc2
				else
					hi2 = h/2
					hi = h/2
				end if
				!print *, 'flag2'
				DG = 0
				do j=1,4
					!print *, pan
					delT = Dt/2*(ti(j)+1)
					p(1) = (xc2-hi)/sqrt(4*nu*delT)
					p(2) = (xc2+hi2)/sqrt(4*nu*delT)
					p(3) = ((yc2-del(i)/2)-hi2)/sqrt(4*nu*delT)
					p(4) = ((yc2-del(i)/2)+hi2)/sqrt(4*nu*delT)
					p(5) = ((yc2+del(i)/2)-hi2)/sqrt(4*nu*delT)
					p(6) = ((yc2+del(i)/2)+hi2)/sqrt(4*nu*delT)

					do k=1,6
						if (abs(p(k))>4.8) then
							erfc(k) = 1-p(k)/abs(p(k))
							
						else
							dist = 10
							do b=1,100
								if (abs(p(k)-erf(b,1))<dist) then
									dist = abs(p(k)-erf(b,1))
									ind = b
									sig = (p(k)-erf(b,1))/abs(p(k)-erf(b,1))
								end if
							enddo
							if (sig > 0) then
								erfc(k) = 1-(erf(ind,2)+(erf(ind+1,2)-erf(ind,2))*(p(k)-erf(ind,1))/(erf(ind+1,1)-erf(ind,1)))
							else
								erfc(k) = 1-(erf(ind,2)+(erf(ind-1,2)-erf(ind,2))*(p(k)-erf(ind,1))/(erf(ind-1,1)-erf(ind,1)))
							end if
						end if
					enddo
					do k=1,4
						ierfc(k)=1/sqrt(pi)*exp(-p(k+2)**2)-p(k+2)*erfc(k+2)
					enddo
					DG = DG+wi(j)*Vt(i)/Dt*((erfc(1)-erfc(2))*1/2*sqrt(4*nu*delT))*((ierfc(1)-ierfc(2))-(ierfc(3)-ierfc(4)))
				enddo
				DG3(counter) = DG*Dt/2
				DG4 = DG4+(DG*Dt/2)**2
				DG5 = DG5+(DG*Dt/2)
				counter = counter+1
			end if
		enddo
		DGN = 0.
        DG = 0.
		do j=1,counter-1
            DGN = DG2(pan(j))+DG3(j)+(DG3(j)**2/DG4)*(del(i)*Vt(i)-DG5)
            DG = DG+DGN
			if (isnan(DGN)) then
                !print *, "we got probz"
            else
                DG2(pan(j)) = DGN
            endif
		enddo
	enddo
	!print *, "Printing Gam"
	!call Print_Data(gamma,nv,1,"gammabn.txt",11,23)
	gamma = gamma + DG2
	!call Print_Data(DG2,nv,1,"check.txt",9,22)
	!call Print_Data(gamma,nv,1,"gamma2n.txt",11,24)
    !call Print_Data(DG2,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vv4.dat",40,8,1)
return
end subroutine Determine_erf