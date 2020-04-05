subroutine Redistribute4(xc,yc,Panels,nv,gamma,vortx,vorty,sigma,mnv,sgam,xp,yp,maxgc,maxgr,gridx,gridy,gridI,xc2,yc2,nu)
	implicit none
	integer :: Panels, nv, i, j, k, count1, count2, mnv, maxgc, maxgr, countv
	real :: xc(Panels+1), yc(Panels+1), sigma(mnv), midx, midy, maxxb, minxb, maxyb, minyb
	real :: vortx(mnv), vorty(mnv), gamma(mnv), minx, maxx, miny, maxy!, xpn(mnv), ypn(mnv), gammast(mnv)
	integer :: row, col, indexx(4), indexy(4), stp, pan(2)
	real :: dist, dx1, dy1, xp(mnv), yp(mnv), xc2(Panels+1), yc2(Panels+1), nu, ycheck(2)
	real, dimension(maxgr,maxgc) :: gridx, gridy, gridI, gamman
	!integer, dimension(ReGridy,ReGridx) :: gridI
	real :: errr, errc, midxp, midyp, ert(mnv), dx2, dy2, gamma2(4), gamma3(4), sgam, ycheck1
	integer, dimension(5,6) :: indexc, indexr
	
	col = 1
	row = 1
	midxp = 0.0
	midyp = 0.0

	indexr = 0.0
	indexc = 0.0

	gamman = 0.

    !call Print_Mat(gridx,maxgr,maxgc,"gx.dat",6,74,0)
    !call Print_Mat(gridy,maxgr,maxgc,"gy.dat",6,75,0)
    !call Print_Mat(gridI,maxgr,maxgc,"gi.dat",6,76,0)
	!call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx9.dat",43,63,0)
	!call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty9.dat",43,64,0)
	!call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma9.dat",43,65,0)    

	do i=1,nv
		dx1 = 100
		dy1 = 100
		do j=1,maxgc-1
			if ((abs(xp(i)-(gridx(1,j)+gridx(1,j+1))/2))/gridI(1,j)<dx1) then
				dx1 = abs(xp(i)-(gridx(1,j)+gridx(1,j+1))/2)/gridI(1,j)
				dx2 = (xp(i)-(gridx(1,j)+gridx(1,j+1))/2)/gridI(1,j)
				col = j
            end if
        enddo
        do j=1,maxgr-1
			if ((abs(yp(i)-(gridy(j,1)+gridy(j+1,1))/2))/gridI(j,1)<dy1) then
				dy1 = abs(yp(i)-(gridy(j,1)+gridy(j+1,1))/2)/gridI(j,1)
				dy2 = (yp(i)-(gridy(j,1)+gridy(j+1,1))/2)/gridI(j,1)
				row = j
			end if
        enddo

        
		gamma3(1) = gamma(i)*(3-2*dx2)*(4*dx2**2-1)/48
		gamma3(2) = gamma(i)*(1-2*dx2)*(9-4*dx2**2)/16
		gamma3(3) = gamma(i)*(1+2*dx2)*(9-4*dx2**2)/16
		gamma3(4) = gamma(i)*(3+2*dx2)*(4*dx2**2-1)/48
       ! print *, col, i
		do j=col-1,col+2
			gamman(row-1,j) = gamman(row-1,j)+gamma3(j-col+2)*(3+2*dy2)*(4*dy2**2-1)/48
			gamman(row,j) = gamman(row,j)+gamma3(j-col+2)*(1+2*dy2)*(9-4*dy2**2)/16
			gamman(row+1,j) = gamman(row+1,j)+gamma3(j-col+2)*(1-2*dy2)*(9-4*dy2**2)/16
			gamman(row+2,j) = gamman(row+2,j)+gamma3(j-col+2)*(3-2*dy2)*(4*dy2**2-1)/48
		enddo

    enddo
	
    sgam = 0.
	count1 = 1
    count2 = 1
	do i=0,maxgc*maxgr-maxgr,maxgr!ReGrid**2-ReGrid,ReGrid
		do j=1,maxgr
            if ((abs(gamman(j,count1))/=0.)) then
			    vortx(count2) = gridx(j,count1)
			    vorty(count2) = gridy(j,count1)
			    gamma(count2) = gamman(j,count1)
                sigma(count2) = gridI(j,count1)
                count2 = count2 + 1
            else
                sgam = sgam+gamman(j,count1)
            endif
            
		enddo
		count1 = count1 + 1
	enddo	
 
	nv = count2-1    
 !   dist = sqrt((xc(2)-xc(1))**2+(yc(2)-yc(1))**2)/4

	maxxb = MAXVAL(xc2)
	minxb = MINVAL(xc2)
	maxyb = MAXVAL(yc2)
	minyb = MINVAL(yc2)
	do i=1,nv
		if (((vortx(i)-maxxb)*(vortx(i)-minxb)<0) .AND. ((vorty(i)-maxyb)*(vorty(i)-minyb)<0)) then
			count2 = 1
			do j=1,Panels
				if ((vortx(i)-xc2(j+1))*(vortx(i)-xc2(j))<0) then
					pan(count2) = j
					count2 = count2+1
				else if ((vortx(i)-xc2(j+1))==0) then
					pan(count2) = j
					count2 = count2+1						
				end if
			enddo
			ycheck(1) = (yc2(pan(1)+1)-yc2(pan(1)))/(xc2(pan(1)+1)-xc2(pan(1)))*(vortx(i)-xc2(pan(1)))+yc2(pan(1));
			ycheck(2) = (yc2(pan(2)+1)-yc2(pan(2)))/(xc2(pan(2)+1)-xc2(pan(2)))*(vortx(i)-xc2(pan(2)))+yc2(pan(2));
			if ((xc2(pan(1)+1)==xc2(pan(1))) .OR. (xc2(pan(2)+1)==xc2(pan(2)))) then
				ycheck(1) = yc2(pan(1));
				ycheck(2) = yc2(pan(2));
            end if
            if (ycheck(1)<ycheck(2)) then
                ycheck1 = ycheck(2)
                ycheck(2) = ycheck(1)
                ycheck(1) = ycheck1
			end if              
			if ((ycheck(1)>=vorty(i)) .AND. (ycheck(2)<=vorty(i))) then
                sgam = sgam+gamma(i)
			else
				vortx(countv) = vortx(i)
				vorty(countv) = vorty(i)
				gamma(countv) = gamma(i)
				countv = countv + 1
			end if
		else
			vortx(countv) = vortx(i)
			vorty(countv) = vorty(i)
			gamma(countv) = gamma(i)
			countv = countv + 1
		end if	
	enddo
	nv = countv-1
 
  !      call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx10.dat",44,61,0)
		!call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty10.dat",44,62,0)
		!call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma10.dat",44,63,0)       
return
end subroutine Redistribute4