subroutine FMM3(vortx,vorty,gamma,nv,box,nb,sigma,Uinf,Dt,nbv,index_vorts,vortx2,vorty2, &
        alpha,points,points2,vx2,vy2,gamma2,sigma2,indexb,Panels,Bn,xci,yci,dthetab,thetab,flag,Rk4x,Rk4y)
	implicit none 
	integer :: nv, i, j, k, count1, lb, lbn, count2, count3, countv, k2, points(10000), lbn2, nb, Panels
	integer, parameter :: max_sub_div = 10, P=9
	integer :: j2, inta, points2(10000), mpbox, indexb(nb)
	real :: gamma(nv), vortx(nv), vorty(nv), maxx, minx, maxy, miny, pi, sigma(nv), Dt, dist, xc2(Panels+1), yc2(Panels+1)
	real :: vortx2(nv), vorty2(nv), gamma2(nv), stp, dx, dy, fk, fk2, fkk2, Uinf(2), sigma2(nv), bn1, bd1, bn2
	real, dimension(nb,13) :: box
	complex, dimension(nb,P+1) :: alpha
	complex :: d, V, V4
	integer :: nbv, index_vorts(nv), k3, ccc, flag
	real :: vx2(nv), vy2(nv), h, vx1, vy1, sig2, vx3, vy3, midx, midy, vn(Panels)!, vx4(nv), vy4(nv)
    real, dimension(Panels,Panels) :: Bn
    real, dimension(nv,4) :: Rk4x, Rk4y
    real :: xci(Panels+1), yci(Panels+1), xc(Panels+1), yc(Panels+1), sourcen(Panels), xp, yp, Vng, Vtg, ssf, csf, dthetab, thetab, thetabk
	
	print *, "FMM Before", nv
	fk = 0.0
	fk2 = 0.0
	fkk2 = 0.0
	alpha = cmplx(0.,0.)
	pi = 3.14159265359
    xc = xci
    yc = yci
    if (flag==1) then
        thetabk=thetab
    else if (flag==2 .OR. flag == 3) then
        thetabk = thetab + Dt/2.*dthetab
    else
        thetabk = thetab + Dt*dthetab
    end if
    
    call Rotate_Body(xc,yc,thetabk,Panels,xc2,yc2,sigma(1))
    
	if (int(nv/5000)<10) then
		mpbox = 10!int(nv/7000)
	else
		mpbox = int(nv/5000)
    end if
    
	maxx = MAXVAL(vortx(1:nv))+0.02
	minx = MINVAL(vortx(1:nv))-0.01
	maxy = MAXVAL(vorty(1:nv))+0.02
	miny = MINVAL(vorty(1:nv))-0.01	
	!print *, box
	if (flag==1) then
	    vortx2 = vortx
	    vorty2 = vorty
        sigma2 = sigma
        gamma2 = gamma
		box = 0.0
		box(1,1:4) = (/minx, (minx+maxx)/2, maxy, (miny+maxy)/2 /)
		box(2,1:4) = (/(minx+maxx)/2, maxx, maxy, (miny+maxy)/2 /)
		box(3,1:4) = (/minx, (minx+maxx)/2, (miny+maxy)/2, miny /)
		box(4,1:4) = (/(minx+maxx)/2, maxx, (miny+maxy)/2, miny /)
	end if
	vx2 = 0.
	vy2 = 0.
	stp = 0
	lb = 1
	lbn = 4
	lbn2 = 4
    !points = 0
    !points2 = 0
    if (flag == 1) then
        indexb = 0
    end if
    
    !indexb = 0
	!vortx = vortx + 0.001
	!vorty = vorty + 0.001
	!print *, "Starting New"
	count3 = 1
	countv = 0
	count1 = 1

	!if (Rk2==1) then
	!	call Print_Data(vortx,nv+1,1,"vortx2.txt",10,7)
	!	call Print_Data(vorty,nv+1,1,"vorty2.txt",10,8)
	!	call Print_Data(gamma,nv+1,1,"gamma2.txt",10,9)
	!end if
	!print *, sigma
	!do i=nv,1,-1
	!	vortx2(count1) = vortx(i)
	!	vorty2(count1) = vorty(i)
	!	count1 = count1 + 1
	!end do
	!vortx = vortx2
	!vorty = vorty2
	!print *, "Start"
	count1 = 1
	if (flag==1) then
		do while (stp==0)
			stp = 1
			do i=lb,lbn2
				count2 = 0
				box(i,12) = count1
				j=1
				do while(j<nv)
                    ccc = 0
					if (((box(i,1)-vortx(j))*(box(i,2)-vortx(j))<=0.) .AND. ((box(i,3)-vorty(j))*(box(i,4)-vorty(j))<=0.)) then
						if ((box(i,1)-vortx(j))*(box(i,2)-vortx(j))==0.) then
                            if ((box(i,1)-vortx(j))==0.) then
                                vortx(j) = vortx(j) + (sigma(j)/2.)
                            else
                                vortx(j) = vortx(j) - (sigma(j)/2.)
           !                 if (vortx(j)==0) then
           !                     vortx(j) = (sigma(j)/2.)
           !                     ccc = 4
           !                 else
           !                     !vortx(j) = vortx(j)+(box(i,1)-vortx(j))*(vortx(j)+sigma(j)/2.)+(box(i,2)-vortx(j))*(vortx(j)-sigma(j)/2.)
							    !vortx(j) = vortx(j)+(sigma(j)/4.)!*vortx(j)/abs(vortx(j))
           !                     ccc = 3
                            end if
                        end if
                        if ((box(i,3)-vorty(j))*(box(i,4)-vorty(j))==0.) then
                            if ((box(i,4)-vorty(j))==0.) then
                                vorty(j) = vorty(j) + (sigma(j)/2.)
                            else
                                vorty(j) = vorty(j) - (sigma(j)/2.)
                            !if (vortx(j)==0) then
                            !    vorty(j) = (sigma(j)/2.)
                            !    ccc = 1
                            !else
                            !    !vorty(j) = (box(i,3)-vorty(j))*(vorty(j)+sigma(j)/2.)+(box(i,4)-vorty(j))*(vorty(j)-sigma(j)/2.)
                            !    vorty(j) = vorty(j)+(sigma(j)/4.)!*vorty(j)/abs(vorty(j))
                            !    ccc = 2
                            end if
                        end if

                        
						!if ((box(i,1)-vortx(j))*(box(i,2)-vortx(j))==0) then
						!	if (vortx(j)==0) then
						!		vortx(j) = 0.000001
						!	else
						!		vortx(j) = vortx(j)+0.000001*vortx(j)/abs(vortx(j))
						!	end if
						!	!print *, "X"
						!else if ((box(i,3)-vorty(j))*(box(i,4)-vorty(j))<=0) then
						!	if (vorty(j)==0) then
						!		vorty(j) = 0.000001
						!	else
						!		vorty(j) = vorty(j)+0.000001*vorty(j)/abs(vorty(j))
						!	end if
						!	!print *, "Y"
						!end if
						count2 = count2 + 1
						points(count2) = j
						if (count2>mpbox) then
							!print *, 3
							lbn = lbn+4
							box(lbn-3,1:4)=(/box(i,1), (box(i,1)+box(i,2))/2, box(i,3), (box(i,3)+box(i,4))/2 /)
							box(lbn-2,1:4)=(/(box(i,1)+box(i,2))/2, box(i,2), box(i,3), (box(i,3)+box(i,4))/2 /)
							box(lbn-1,1:4)=(/box(i,1), (box(i,1)+box(i,2))/2, (box(i,3)+box(i,4))/2, box(i,4) /)
							box(lbn,1:4)=(/(box(i,1)+box(i,2))/2, box(i,2), (box(i,3)+box(i,4))/2, box(i,4) /)
							box(lbn-3,7) = i
							box(lbn-2,7) = i
							box(lbn-1,7) = i
							box(lbn,7) = i						
							box(i,8:11) = (/lbn-3, lbn-2, lbn-1, lbn /)
							stp = 0
							j = nv+1
						end if
					end if
					j = j+1
				end do
				if ((count2<=mpbox) .AND. (count2/=0)) then
					do k=1,count2
						if (k+countv<=nv) then
							vortx2(k+countv) = vortx(points(k))
							vorty2(k+countv) = vorty(points(k))
							gamma2(k+countv) = gamma(points(k))
                            sigma2(k+countv) = sigma(points(k))
							!ux02(k+countv,:) = (/ux0(points(k),1), ux0(points(k),2)/)

                            if (sqrt((box(i,1)+box(i,2))**2+(box(i,3)+box(i,4))**2)>0.7) then
                                box(i,13) = 1.
                                !sigma(k+countv,:)=
                            else
                                box(i,13) = 0.
                            endif
						!else
							!print *, "Too Many Vorts", nv, k+countv
						end if
					end do
					if (k+countv<=nv) then
						box(i,5:6) = (/ countv+1, countv+count2/)
		              !  do k3=0,P
			             !   alpha(i,k3+1) = cmplx(0.,0.)
				            !do k2=countv+1,countv+count2
					           ! dx=vortx2(k2)-(box(i,1)+box(i,2))/2
					           ! dy=vorty2(k2)-(box(i,3)+box(i,4))/2					
					           ! d = cmplx(dx,dy)
					           ! alpha(i,k3+1) = alpha(i,k3+1)+gamma2(k2)*d**(k)
				            !end do
		              !  end do	                        
                        indexb(i) = 1
                        nb = i
					else
						box(i,5:6) = (/ countv+1, nv/)
		              !  do k3=0,P
			             !   alpha(i,k3+1) = cmplx(0.,0.)
				            !do k2=countv+1,nv
					           ! dx=vortx2(k2)-(box(i,1)+box(i,2))/2
					           ! dy=vorty2(k2)-(box(i,3)+box(i,4))/2					
					           ! d = cmplx(dx,dy)
					           ! alpha(i,k3+1) = alpha(i,k3+1)+gamma2(k2)*d**(k)
				            !end do
		              !  end do
                        indexb(i) = 1
                        nb = i
					end if

						
					countv = countv+count2
					index_vorts(count3) = i
					count3 = count3 + 1

				end if
			end do
			
			lb = i
			!print *, i,lb
			lbn2 = lbn
			count1 = count1 + 1

		end do

	end if
	if (flag==1) then
		nbv = count3-1
		nv=int(MAXVAL(box(:,6)))
		nb = i-1
    
	    !----- This loop calculates all of the alpha values -----
	    do j=1,nb
		    do k=0,P
			    alpha(j,k+1) = cmplx(0.,0.)
			    if (int(box(j,5))/=0) then
				    do k2=int(box(j,5)),int(box(j,6))
					    dx=vortx2(k2)-(box(j,1)+box(j,2))/2
					    dy=vorty2(k2)-(box(j,3)+box(j,4))/2					
					    d = cmplx(dx,dy)

					    alpha(j,k+1) = alpha(j,k+1)+gamma2(k2)*d**(k)
 
				    end do
			    end if
		    end do
	    end do
	    !----- This loop calculates all of the parents alpha values -----
	    do j=nb,1,-1
		    if (int(box(j,7))/=0) then
		    !print *, j
			    do k=0,P
				    if (k==0) then
					    fk = 1.
				    else
					    fk=fk*k
                    end if
                    inta = int(box(j,7))
				    dx=(box(j,1)+box(j,2))/2.-(box(inta,1)+box(inta,2))/2.
				    dy=(box(j,3)+box(j,4))/2.-(box(inta,3)+box(inta,4))/2.
				    d = cmplx(dx,dy)  
                    fk2 = 1.
                    do k2=0,k
                        if (k2==0) then
                            fk2 = 1.
                        else
                            fk2 = fk2*(k2)
                        end if
                    
                        bn2 = 1.
                        do i=0,(k-k2)
                            if (i==0) then
                                bn2 = 1.
                            else
                                bn2 = bn2*(i)
                            end if
                        end do
                        alpha(inta,k+1) = alpha(inta,k+1)+alpha(j,k2+1)*fk/(bn2*fk2)*d**(k-k2)
                    end do
			    end do
		    end if
	    end do
    end if
    vx2 = 0.
    vy2 = 0.

    print *, "Velo"

    !$OMP PARALLEL DO PRIVATE(points,points2,dx,dy,count1,count3,inta,d,fk,fk2,V,vx1,vy1,sig2)! SHARED(vx2,vy2)
	do j=1,nb
        if (indexb(j)==1) then
		    points2(1:4) = (/1, 2, 3, 4 /)
		    count3=4
            vx1 = 0.
            vy1 = 0.
		    do j2=1,int(box(nb-1,12))
			    points=points2
			    count1=1
			    do i=1,count3
				    inta = points(i)
				    dx = (box(j,1)+box(j,2))/2-(box(inta,1)+box(inta,2))/2
				    dy = (box(j,3)+box(j,4))/2-(box(inta,3)+box(inta,4))/2
				    d = cmplx(dx,dy)
				    fk = sqrt(dx**2+dy**2)
				    fk2 = sqrt((box(inta,2)-box(inta,1))**2+(box(inta,3)-box(inta,4))**2)/2. + sqrt((box(j,2)-box(j,1))**2+(box(j,3)-box(j,4))**2)/1.9

				    if (fk>=fk2) then
					    V=0.
					    do k=0,P
						    V=V+alpha(inta,k+1)/d**k
                        end do
                        V = V*cmplx(dy/(2.*pi*(dx**2+dy**2)),dx/(2.*pi*(dx**2+dy**2)))
					    !V=V*cmplx(0.0,1.0/(2.0*pi))*1/d
                        
					    vx1=vx1-real(V)
					    vy1=vy1+imag(V)
				    else if (indexb(inta)==0) then
					    if (int(box(inta,8))/=0) then
						    points2(count1:count1+3)=int(box(inta,8:11))
						    count1=count1+4
						    !print *, count1
					    end if
                    else
                        do k2=int(box(j,5)),int(box(j,6))
					        do k=int(box(inta,5)),int(box(inta,6))
                                if (k2/=k) then
						            dx = vortx2(k2)-vortx2(k)
						            dy = vorty2(k2)-vorty2(k)
						            fk = sqrt(dx**2+dy**2)
                                    if (fk/=0.) then
                                        sig2 = sqrt((sigma(k)**2.+sigma(k2)**2.))/2.
							            fk2=1/(2*pi)*(1-exp(-((fk/sig2)**2.)/2.))
							            vx2(k2)=vx2(k2)-(fk2/fk**2)*dy*gamma2(k)
							            vy2(k2)=vy2(k2)+(fk2/fk**2)*dx*gamma2(k)
                                    end if
                                    
                                end if
                            end do
                        end do
				    end if
			    end do
			    count3=count1-1
            end do
            vx2(int(box(j,5)):int(box(j,6)))=vx2(int(box(j,5)):int(box(j,6)))+vx1
            vy2(int(box(j,5)):int(box(j,6)))=vy2(int(box(j,5)):int(box(j,6)))+vy1
        end if
    end do
    !$OMP END PARALLEL DO
    
!!--- This satisfies the no normal flow --------------
    !$OMP PARALLEL DO PRIVATE(midx,midy,vx3,vy3,dx,dy,fk2,csf,ssf)
    do i=1,Panels
        midx = (xc(i+1)+xc(i))/2.
        midy = (yc(i+1)+yc(i))/2.
        vx3 = 0.
        vy3 = 0.
        do j=1,nv
            dx = midx-vortx2(j)
            dy = midy-vorty2(j)
            fk2 = dx**2.+dy**2.
            vx3 = vx3 - gamma2(j)/(2.*pi)*dy/fk2
            vy3= vy3 + gamma2(j)/(2.*pi)*dx/fk2
        end do
        vx3 = vx3+Uinf(1)+midy*dthetab
        vy3 = vy3+Uinf(2)-midx*dthetab
        dx = xc(i+1)-xc(i)
        dy = yc(i+1)-yc(i)
        dist = sqrt(dx**2.+dy**2.)
        csf = dx/dist
        ssf = dy/dist
        vn(i) = -(vx3*ssf - vy3*csf)
    end do
    !$OMP END PARALLEL DO
    sourcen = matmul(Bn,vn)
    !vx3 = 0.
    !IN THIS LOOP
    !$OMP PARALLEL DO PRIVATE(dx,dy,dist,vx3,vy3,csf,ssf,xp,yp,midx,midy,Vng,Vtg)
    do i=1,nv
        vx3 = 0.
        vy3 = 0.
        do j=1,Panels
            dx = xc(j+1)-xc(j)
            dy = yc(j+1)-yc(j)
            dist = sqrt(dx**2.+dy**2.)
            csf = dx/dist
            ssf = dy/dist
		    midx = (xc(j+1)+xc(j))/2.
	    	midy = (yc(j+1)+yc(j))/2.
            xp = ((vortx2(i)-xc(j))*csf + (vorty2(i)-yc(j))*ssf)
            yp = ((vortx2(i)-xc(j))*ssf - (vorty2(i)-yc(j))*csf)
            Vtg = -sourcen(j)/(4.*pi)*LOG(((xp-dist)**2.+yp**2.)/(xp**2.+yp**2.))
            Vng = -sourcen(j)/(2.*pi)*atan2(-yp*dist,yp**2.+xp**2.-dist*xp)

            vx3 = vx3+Vng*ssf+Vtg*csf!-Vtg*ssf+Vng*csf
            vy3 = vy3-Vng*csf+Vtg*ssf!+Vtg*csf+Vng*ssf
        end do
       ! vx4(i) = vx3
        !vy4(i) = vy3
        vx2(i) = vx2(i)+vx3
        vy2(i) = vy2(i)+vy3
    end do
    !$OMP END PARALLEL DO
    
	vx2 = vx2+Uinf(1)
	vy2 = vy2+Uinf(2)
    Rk4x(1:nv,flag) = Dt*vx2(1:nv)
    Rk4y(1:nv,flag) = Dt*vy2(1:nv)

    if (flag == 1) then
	    vortx(1:nv) = vortx2(1:nv)
	    vorty(1:nv) = vorty2(1:nv)
	    gamma(1:nv) = gamma2(1:nv)
        sigma(1:nv) = sigma2(1:nv)
    end if
    
    if (flag==2 .OR. flag==3) then
        vortx2 = vortx + Rk4x(:,flag-1)/2.
        vorty2 = vorty + Rk4y(:,flag-1)/2.
    else if (flag==4) then
        vortx2 = vortx + Rk4x(:,flag-1)
        vorty2 = vorty + Rk4y(:,flag-1)
    end if
 
	! Issue is in this function
	call Print_Data(vortx2,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx1.dat",43,63,1)
	call Print_Data(vorty2,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty1.dat",43,64,1)
	call Print_Data(gamma2,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma1.dat",43,65,1)
   
 !   call Print_Data(vx4,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/sigma9.dat",43,57,1) 	
 !   call Print_Data(vy4,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx10.dat",44,51,1)
	call Print_Data(vx2,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty15.dat",44,52,1)
	call Print_Data(vy2,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma15.dat",44,53,1)
    !call Print_Data(sourcen,Panels,1,"D:/bpoll/Fortran/Large_Data/Foil/sigma10.dat",44,54,1)
 !   call Print_Data(xc,Panels+1,1,"D:/bpoll/Fortran/Large_Data/Foil/sigma11.dat",44,50,1)
 !   call Print_Data(yc,Panels+1,1,"D:/bpoll/Fortran/Large_Data/Foil/sigma12.dat",44,49,1)
    
	print *, "FMM After", nv
return
end subroutine FMM3