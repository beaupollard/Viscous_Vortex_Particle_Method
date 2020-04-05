program MAIN_SOLID
	use MAIN_INIT
	implicit none
	
    rad_C = 0.144!0.18! 0.5!mtheta*Uinf(1)
	Uinf(2) = 0.!550*nu
	Uinf(1) = 1000.*nu/rad_C
    mtheta = 2.5/.75*Uinf(1)!4.*Uinf(1)!0.!2.5/.75*Uinf(1)
    
	Dt = 2.*rad_C*T/Uinf(1)
    Bt_S = 0.
	print *, nu, Uinf(1), Dt
	nb = 10000
	sgam = 0.
	open(unit=2, file="erf.txt")
	do i=1,100
		read(2,*) erf(i,1), erf(i,2)
	enddo
	Flag = 0
    sigma = h
    do i=1,maxI
        dthetab(i) = mtheta*sin(2.*pi*Uinf(1)/(2.*rad_C)*dT*(i-1))
        !dthetab(i) = mtheta*sin(0.1*pi*dT*(i-1))
       ! dthetab(i) = mtheta*sin(pi*dT*(i-1))
    enddo
    !thetab = -15.*pi/180.
    !dthetab = 0.
    do i=2,maxI
        thetab(i) = thetab(i-1) + dthetab(i)*Dt
    end do
    thetab = thetab-maxval(thetab)/2.
    !
!!--- Create the cylinder and initial vortex field ------
	!call Create_Shape(xc,yc,Panels,rad_C,vortx,vorty,h,gamma,mnv)
	!maxxb = MAXVAL(xc)
	!minxb = MINVAL(xc)
	!maxyb = MAXVAL(yc)
	!minyb = MINVAL(yc)
 !   xci = xc
 !   yci = yc
	!nv = 0
	!print *, 'Remove Zeros'
	!do i=1,mnv
	!	IF (vortx(i)/=0. .OR. vorty(i)/=0.) THEN
	!		nv = nv+1;
	!		ux0(nv,:) = (/0., 0./)
	!	END IF
 !   enddo  
 !   call Grid_Create2(xc,yc,gridx,gridy,gridI,Panels,maxgrid,h,gx,gy)
!--- Comment this out if you dont want to use a foil ----------    
	open(unit=3, file="foil5.txt")
    !open(unit=3, file="square.txt")
	do i=1,Panels+1
		read(3,*) xc(i), yc(i)
    enddo
 
    xc = xc-0.25
    xci = xc
    yci = yc
    !thetab-maxval(thetab)/2.
    call Rotate_Body(xc,yc,thetab(1),Panels,xc2,yc2,h)
	maxxb = MAXVAL(xc)
	minxb = MINVAL(xc)
	maxyb = MAXVAL(yc)
	minyb = MINVAL(yc)
    call Grid_Create2(xc,yc,gridx,gridy,gridI,Panels,maxgrid,h,gx,gy)
    nv = mnv
    call Place_Vorts(vortx,vorty,xc2,yc2,h,nv,Panels,xpn,ypn)
    
    area = 0.
	do i=1,Panels
		area = area+1./2.*(xc(i)*yc(i+1)-xc(i+1)*yc(i))
    enddo

!--- Create the matrix Bt ------------------------------	
	print *, 'Panel Influence'
	call Panel_Influence(xc,yc,Panels,Bt,Bn)
    
    Bt(1:Panels,:) = -Bt(1:Panels,:)
    !
    !call Print_Mat(gridx,maxgrid,maxgrid,"Bn.dat",6,35,0)
    BtT = transpose(Bt(1:Panels,:))
    
    Bt_S(1:Panels,1:Panels) = 2.*matmul(BtT,Bt(1:Panels,:))
   
    do i=1,Panels
        Bt_S(i,Panels+1) = Bt(Panels+1,i)
        Bt_S(Panels+1,i) = Bt(Panels+1,i)
    enddo
    !Bt_S(Panels+1,Panels+1) = Bt(Panels+1,1)
    
    i = Panels+1
    call sgetrf(Panels+1,Panels+1,Bt_S,i,ipiv,info2)
    i = Panels+1
    info2 = 0
    LWORK = 2*Panels+2
    call sgetri(i,Bt_S,i,ipiv,work2,LWORK,info2)
    !call Print_Mat(Bt_S,Panels+1,Panels+1,"Bts.dat",7,74,0)
    !--- For the normal with potentials --------
    Bni = Bn
    i = Panels
    call sgetrf(Panels,Panels,Bn,i,ipiv(1:Panels),info2(1:Panels))
    i = Panels
    info2 = 0
    LWORK = 2*Panels
    call sgetri(i,Bn,i,ipiv(1:Panels),work2,LWORK,info2(1:Panels))
    
    
	Bt2 = Bt
    xc2 = xc*(1.+h/2.)
	yc2 = yc*(1.+h/2.)

	j = 0
	counter = 0
    !nv = mnv
    !call Import_Vorts(vortx(nv),vorty(nv),gamma(nv),nv)
 !   nv = 500452
 !   print *, nv
 !   open(unit=3, file="Vxn.dat")
	!do i=1,nv
	!	read(3,*) vortx(i)
 !   enddo
 !   print *, nv
 !   open(unit=4, file="Vyn.dat")
	!do i=1,nv
	!	read(4,*) vorty(i)
 !   enddo   
 !   open(unit=5, file="gan.dat")
	!do i=1,nv
	!	read(5,*) gamma(i)
 !   enddo  
 !    j2 = 1
 !   j2 = 1
 !   ux0 = 0.
     !   call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx5.dat",43,37,0)
	    !call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty5.dat",43,32,0)
	    !call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma5.dat",43,33,0)    
    !call Print_Data(vortx,nv,1,"vortx9.dat",10,7,0)
	!call Print_Data(vorty,nv,1,"vorty9.dat",10,8,0)
	!call Print_Data(gamma,nv,1,"gamma9.dat",10,9,0)
	call CPU_TIME(time)
    
        !call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx10.dat",44,61,0)
		!call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty10.dat",44,62,0)
		!call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma10.dat",44,63,0)
countP= 0   
do k=1,maxI!k=1,maxI
    Bt = Bt2
    !j2=1

    if (k>1) then
        ddthetab = 2*area*(dthetab(k+1)-dthetab(k))
    else
        ddthetab = 2*area*(dthetab(2)-dthetab(1))
    endif
    
	Bt = Bt2
	print *, k
	call CPU_TIME(time2)
	print *, time2-time
	call CPU_TIME(time)
!--- Move the vorticies ---------------------------------		
	if (k>1) then

		if (j2==0) then	
			nb = mnv
			!nb = 5*nv
	        print *, "FMM Before", nv
			!call FMM3(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,box(1:nb,:),nb,h,ux0(1:nv,:),Uinf,0,Dt,nbv,index_vorts, &
			!xpn(1:nv),ypn(1:nv),ddthetab,xc,yc,Panels)
	  !
			!print *, "PSE"
   !
			!call PSE6(xc,yc,Panels,nv,gamma(1:nv),vortx(1:nv),vorty(1:nv),h,nu,Dt,box(1:nb,:),index_vorts(1:nbv),nb,nbv)
            
! ----- TODO this block of code is going to be a RK4 solver for the vortex positions and I need to work on it later ---------------            
        !    do i=1,4
 			    !call FMM3(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,box(1:nb,:),nb,sigma(1:nv),Uinf,Dt,nbv,index_vorts(1:nv), &
        !            vortxp(nv),vortyp(nv),alpha_box(1:nb,:),points(20000),points2(20000),vxst(nv),vyst(nv),gammast(nv),sigma2(1:nv),indexb(nb), &
        !            Panels,Bn,xci,yci,dthetab(k),thetab(k-1),i,Rk4x(1:nv,:),Rk4y(1:nv,:))
        !    enddo
        !    !call Print_Mat(Rk4x(1:nv),nv,4,"Rk4x.dat",8,74,0)
        !    !call Print_Mat(Rk4y(1:nv),nv,4,"Rk4y.dat",8,75,0)   
        !    xpn(1:nv) = vortx(1:nv)+1./6.*(Rk4x(1:nv,1)+2.*Rk4x(1:nv,2)+2.*Rk4x(1:nv,3)+Rk4x(1:nv,4))
        !    ypn(1:nv) = vorty(1:nv)+1./6.*(Rk4y(1:nv,1)+2.*Rk4y(1:nv,2)+2.*Rk4y(1:nv,3)+Rk4y(1:nv,4))
            
            
			call FMM2(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,box(1:nb,:),nb,sigma(1:nv),ux0(1:nv,:),Uinf,0,Dt,nbv,index_vorts(1:nv),xpn(1:nv),ypn(1:nv), &
                ux02(1:nv,:),vortxp(1:nv),vortyp(1:nv),alpha_box(1:nb,:),points(1:20000),points2(1:20000),vxst(1:nv),vyst(1:nv),gammast(1:nv),sigma2(1:nv),indexb(1:nb),Panels,Bn,xc,yc,dthetab(k))
            !call Print_Mat(box(1:nb,:),nb,13,"box.dat",7,75,0)
            !call Print_Mat(ux0(1:nv,:),nv,2,"ux1.dat",7,40,0) 
			print *, "PSE"
            !call PSE5(xc,yc,Panels,nv,gamma(1:nv),vortx(1:nv),vorty(1:nv),h,nu,Dt,box(1:nb,:),index_vorts(1:nbv),nb,nbv)
            call PSE4(xc,yc,Panels,nv,gamma(1:nv),vortx(1:nv),vorty(1:nv),sigma(1:nv),nu,Dt,box(1:nb,:),index_vorts(1:nbv),nb,nbv)
            
	    !call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx9.dat",43,63,0)
	    !call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty9.dat",43,64,0)
	    !call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma9.dat",43,65,0)    
     !   call Print_Data(sigma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/sigma9.dat",43,57,0) 	
			mxg =  MAXVAL(abs(gamma(1:nv)))
			countv = 1
			print *, "Reduce # Vorts"
			pan = 0
			sgam = 0.

		else
			print *, 'MV1'
			nb = mnv

			call FMM2(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,box(1:nb,:),nb,sigma(1:nv),ux0(1:nv,:),Uinf,1,Dt,nbv,index_vorts(1:nv),xpn(1:nv),ypn(1:nv), &
                ux02(1:nv,:),vortxp(1:nv),vortyp(1:nv),alpha_box(1:nb,:),points(1:20000),points2(1:20000),vxst(1:nv),vyst(1:nv),gammast(1:nv),sigma2(1:nv),indexb(1:nb),Panels,Bn,xc,yc,dthetab(k))
            !call Print_Mat(box(1:nb,:),nb,13,"D:/bpoll/Fortran/Large_Data/Foil/box.dat",40,7,0)
           
			print *, "PSE"
			call PSE4(xc,yc,Panels,nv,gamma(1:nv),xpn(1:nv),ypn(1:nv),sigma(1:nv),nu,Dt,box(1:nb,:),index_vorts(1:nbv),nb,nbv)

			print *, 'MV2'	
			call FMM2(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,box(1:nb,:),nb,sigma(1:nv),ux0(1:nv,:),Uinf,2,Dt,nbv,index_vorts(1:nv),xpn(1:nv),ypn(1:nv), &
               ux02(1:nv,:),vortxp(1:nv),vortyp(1:nv),alpha_box(1:nb,:),points(1:20000),points2(1:20000),vxst(1:nv),vyst(1:nv),gammast(1:nv),sigma2(1:nv),indexb(1:nb),Panels,Bn,xc,yc,dthetab(k))

			mxg =  MAXVAL(abs(gamma(1:nv)))
			countv = 1	
            
			print *, "Reduce # Vorts"
			pan = 0
			
			j2 = 0
		end if
    end if
    !sgam = 0.
    !count3 = 0
    xc = xci
    yc = yci    
    call Rotate_Body(xc,yc,thetab(k),Panels,xc2,yc2,h)
	if (k>1) then
        countv = 1
       ! call Remove_Vorts(Panels,nv,xc2,yc2,xpn,ypn,gamma,vortx,vorty,sgam,ux0,nu,0.00001,1)
	    maxxb = MAXVAL(xc2)
	    minxb = MINVAL(xc2)
	    maxyb = MAXVAL(yc2)
	    minyb = MINVAL(yc2)
		do i=1,nv
			if (((xpn(i)-maxxb)*(xpn(i)-minxb)<0) .AND. ((ypn(i)-maxyb)*(ypn(i)-minyb)<0)) then
				count2 = 1
				do j=1,Panels
					if ((xpn(i)-xc2(j+1))*(xpn(i)-xc2(j))<0) then
						pan(count2) = j
						count2 = count2+1
					else if ((xpn(i)-xc2(j+1))==0) then
						pan(count2) = j
						count2 = count2+1						
					end if
				enddo
				ycheck(1) = (yc2(pan(1)+1)-yc2(pan(1)))/(xc2(pan(1)+1)-xc2(pan(1)))*(xpn(i)-xc2(pan(1)))+yc2(pan(1));
				ycheck(2) = (yc2(pan(2)+1)-yc2(pan(2)))/(xc2(pan(2)+1)-xc2(pan(2)))*(xpn(i)-xc2(pan(2)))+yc2(pan(2));
				if ((xc2(pan(1)+1)==xc2(pan(1))) .OR. (xc2(pan(2)+1)==xc2(pan(2)))) then
					ycheck(1) = yc2(pan(1));
					ycheck(2) = yc2(pan(2));
                end if
                if (ycheck(1)<ycheck(2)) then
                    ycheck1 = ycheck(2)
                    ycheck(2) = ycheck(1)
                    ycheck(1) = ycheck1
			    end if                  
				if ((ycheck(1)>=ypn(i)) .AND. (ycheck(2)<=ypn(i))) then
                    sgam = sgam+gamma(i)
				else if (abs(gamma(i))/nu>=0.0001) then
					vortx(countv) = xpn(i)
					vorty(countv) = ypn(i)
					gamma(countv) = gamma(i)
					ux0(countv,:) = ux0(i,:)
					countv = countv + 1
                else
                    sgam = sgam+gamma(i)
                    count3 = count3 + 1
				end if
			else if (abs(gamma(i))/nu>=0.0001) then
				vortx(countv) = xpn(i)
				vorty(countv) = ypn(i)
				gamma(countv) = gamma(i)
				ux0(countv,:) = ux0(i,:)
				countv = countv + 1
            else
                sgam = sgam+gamma(i)
                count3 = count3 + 1
			end if	
		enddo
		nv = countv-1
        
        if (countP==2) then
            call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/FoilxR4.dat",44,37,1)
	        call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/FoilyR4.dat",44,32,1)
	        call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gammFR4.dat",44,33,1)   
	        call Print_Data(xc,Panels+1,1,"D:/bpoll/Fortran/Large_Data/Foil/xFR4.dat",41,58,1)
	        call Print_Data(yc,Panels+1,1,"D:/bpoll/Fortran/Large_Data/Foil/yFR4.dat",41,59,1)
            countP = 0
        else
            countP = countP+1
        end if
        
    end if
  !      call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx10.dat",44,61,0)
		!call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty10.dat",44,62,0)
		!call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma10.dat",44,63,0)    
	    !call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx9.dat",43,63,1)
	    !call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty9.dat",43,64,1)
	    !call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma9.dat",43,65,1)    
     !   call Print_Data(sigma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/sigma9.dat",43,57,1) 	
	print *, "Vort Affects"
    !if (k==275) then
	   ! call Print_Data(vortx,nv,1,"vortx6.dat",10,7,0)
	   ! call Print_Data(vorty,nv,1,"vorty6.dat",10,8,0)	
	   ! call Print_Data(gamma,nv,1,"gamma6.dat",10,9,0)
    !end if
    

!--- Calculate effects from surrounding vorticies -----------
	call Affects_Vorts_Pot(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,xc,yc,Panels,Vt,Uinf,sgam,Vn,ddthetab,dthetab(k),vt_ie,0)

!--- Calculate the vortex surrounding the cylinder ----------
    Vt2 = 2*matmul(BtT,Vt)
    vt_i(1:Panels) = Vt2
    vt_i(Panels+1) = vt_ie
    !call Print_Data(Vt2,Panels,1,"Vti.dat",7,11,0)
    DG = matmul(Bt_S,vt_i)
    !call Force_Calc(vortx(1:nv),vorty(1:nv),gamma(1:nv),xc,yc,DG,nv,Panels,Fx(k),Fy(k),Fx2,Fy2,sigma(nv),Fx4,Fy4,Fx3(k),Fy3(k),nu,Dt,area,ddthetab,Uinf)        
!--- Determine the flux from the wake panels surrounding the body -----
	call Determine_erf(xc,yc,Panels,vortx(1:nv),vorty(1:nv),h,gamma(1:nv),nv,erf,DG,nu,Dt)

    !call Affects_Vorts_Pot(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,xc,yc,Panels,Vt,Uinf,sgam,Vn,ddthetab,dthetab(k),vt_ie,0)
	!mxg = MAXVAL(abs(gamma(1:nv)))
    
	Ix(k) = 0.0
    Iy(k) = 0.0
	do i=1,nv
		Ix(k)=Ix(k)+gamma(i)*vorty(i)
        Iy(k)=Iy(k)+gamma(i)*vortx(i)
    enddo
    
    ! Calculate the forces from Eldredge paper----------
    !call Force_Calc(vortx(1:nv),vorty(1:nv),gamma(1:nv),xc,yc,DG,nv,Panels,Fx(k),Fy(k),Fx2,Fy2,sigma(nv),Fx4,Fy4,Fx3(k),Fy3(k),nu,Dt,area,ddthetab,Uinf)    
    !call Print_Data(Ix(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/IxC550.dat",43,79,0)
    !call Print_Data(Iy(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/IyC550.dat",43,78,0)
    !call Print_Data(Fx(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/FxC550.dat",43,71,0)
    !call Print_Data(Fy(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/FyC550.dat",43,72,0) 
    call Print_Data(Ix(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/IxF2000.dat",44,79,0)
    call Print_Data(Iy(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/IyF2000.dat",44,78,0)
    !call Print_Data(Fx(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/FxF550_2.dat",45,71,0)
    !call Print_Data(Fy(1:k),k,1,"D:/bpoll/Fortran/Large_Data/Foil/FyF550_2.dat",45,72,0)
	if (counter==4) then
		print *, 'Grid'
		call Grid_Create(nv,gamma,vortx,vorty,h,mx,my,ReGrid)
		!print *, 'Redistribute'
		!if (Flag == 0) then
        print *, 'redis'
		!end if
		call Redistribute3(xc,yc,Panels,nv,gamma,vortx,vorty,h,ReGrid,mx,my,mnv,sgam,xpred,ypred)        
!! -------- New Redis -----------------
!	    maxvx = MAXVAL(vortx(1:nv))
!	    minvx = MINVAL(vortx(1:nv))
!	    maxvy = MAXVAL(vorty(1:nv))
!	    minvy = MINVAL(vorty(1:nv)) 
!        
!        do i=1,maxgrid-1
!            if (gx(i) .LE. minvx .AND. gx(i+1) > minvx) then
!                minvxi = i-5
!            endif
!            if (gx(i) .LE. maxvx .AND. gx(i+1) > maxvx) then
!                maxvxi = i+5
!            endif
!            if (gy(i+1) .LE. minvy .AND. gy(i) > minvy) then
!                maxvyi = i+5
!            endif
!            if (gy(i+1) .LE. maxvy .AND. gy(i) > maxvy) then
!                minvyi = i-5
!            endif          
!        enddo
!        if (minvyi < 6) then
!            minvyi = 5
!        endif
!        if (maxvyi > maxgrid-5) then
!            maxvyi = maxgrid-5
!        endif
!        if (minvxi < 6) then
!            minvxi = 5
!        endif
!        if (maxvxi > maxgrid-5) then
!            maxvxi = maxgrid-5
!        endif
!        maxgr = maxvyi-minvyi+1
!        maxgc = maxvxi-minvxi+1
!        xpred = vortx
!        ypred = vorty
!		!call Print_Data(vortx,nv,1,"vortxb3.dat",11,10,0)
!		!call Print_Data(vorty,nv,1,"vortyb3.dat",11,11,0)
!        !call Print_Data(gamma,nv,1,"gammab3.dat",11,12,0)
!        !call Print_Mat(gridx(minvyi:maxvyi,minvxi:maxvxi),maxgr,maxgc,"gx.dat",6,34,0)
!        !call Print_Mat(gridy(minvyi:maxvyi,minvxi:maxvxi),maxgr,maxgc,"gy.dat",6,35,0)
!        call Redistribute4(xc,yc,Panels,nv,gamma,vortx,vorty,sigma,mnv,sgam,xpred,ypred,maxgc,maxgr, &
!            gridx(minvyi:maxvyi,minvxi:maxvxi),gridy(minvyi:maxvyi,minvxi:maxvxi),gridI(minvyi:maxvyi,minvxi:maxvxi),xc,yc,nu)
!        
!    !--- Calculate effects from surrounding vorticies -----------
	    call Affects_Vorts_Pot(vortx(1:nv),vorty(1:nv),gamma(1:nv),nv,xc,yc,Panels,Vt,Uinf,sgam,Vn,ddthetab,dthetab(k),vt_ie,0)
        sgam = 0.
    !!--- Calculate the vortex surrounding the cylinder ----------	
    
        Vt2 = 2*matmul(BtT,Vt)
        vt_i(1:Panels) = Vt2
        vt_i(Panels+1) = vt_ie
    
        DG = matmul(Bt_S,vt_i)
        
	    print *, 'Determine_ERF'
    !--- Determine the flux from the wake panels surrounding the body -----
	    !call Print_Data(vortx,nv,1,"vortxb.txt",10,10)
	    !call Print_Data(vorty,nv,1,"vortyb.txt",10,11)
	    !call Print_Data(gamma,nv,1,"gammab3.txt",11,12)	
	    call Determine_erf(xc,yc,Panels,vortx(1:nv),vorty(1:nv),h,gamma(1:nv),nv,erf,DG,nu,Dt)
  !
        call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx6.dat",43,37,0)
	    call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty6.dat",43,32,0)
	    call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma6.dat",43,33,0)
		counter = -1
		j2 = 1

    end if

	print *, nv
	counter = counter+1
enddo


end