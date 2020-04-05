subroutine Redistribute3(xc,yc,Panels,nv,gamma,vortx,vorty,h,ReGrid,mx,my,mnv,sgam,xp,yp)
	implicit none
	integer :: Panels, nv, i, j, k, ReGrid, count1, count2, mnv, countv
	real :: xc(Panels+1), yc(Panels+1), h, midx, midy, ycheck(2), ycheck1, maxxb, minxb, maxyb, minyb
	real :: vortx(mnv), vorty(mnv), gamma(mnv), minx, maxx, miny, maxy
	integer :: row, col, indexx(4), indexy(4), stp, pan(2)
	real :: sigy, sigx, dist, my, mx, dx1, dy1, xp(mnv), yp(mnv), xp2(ReGrid*Regrid), yp2(ReGrid*Regrid), xc2(Panels+1), yc2(Panels+1)
	real, dimension(ReGrid,ReGrid) :: gridx, gridy, gamman
	!integer, dimension(ReGrid,ReGrid) :: gridI
	real :: errr, errc, midxp, midyp, gamma2n(ReGrid**2), ert(mnv), dx2, dy2, gamma2(4), gamma3(4), sgam
	integer, dimension(5,6) :: indexc, indexr
	
	col = 1
	row = 1
	midxp = 0.0
	midyp = 0.0
	sigx = 1.0
	sigy = 1.0
	xc = xc/h
	yc = yc/h
	xp = vortx/h
	yp = vorty/h
	indexr = 0.0
	indexc = 0.0
	do i=1,ReGrid
		do j=1,ReGrid
			gridy(j,i) = my-j
			gridx(j,i) = mx+i
			!if (sqrt(gridx(j,i)**2+gridy(j,i)**2)<=0.5/h) then
			!	gridI(j,i) = 0
			!else
			!	gridI(j,i) = 1
			!end if
			gamman(i,j) = 0.
		enddo
	enddo

	do i=1,nv
		dx1 = 100
		dy1 = 100
		do j=1,ReGrid-1
			if (abs(xp(i)-(gridx(1,j)+gridx(1,j+1))/2)<dx1) then
				dx1 = abs(xp(i)-(gridx(1,j)+gridx(1,j+1))/2)
				dx2 = (xp(i)-(gridx(1,j)+gridx(1,j+1))/2)
				col = j
			end if
			if (abs(yp(i)-(gridy(j,1)+gridy(j+1,1))/2)<dy1) then
				dy1 = abs(yp(i)-(gridy(j,1)+gridy(j+1,1))/2)
				dy2 = (yp(i)-(gridy(j,1)+gridy(j+1,1))/2)
				row = j
			end if
		enddo
		gamma3(1) = gamma(i)*(3-2*dx2)*(4*dx2**2-1)/48
		gamma3(2) = gamma(i)*(1-2*dx2)*(9-4*dx2**2)/16
		gamma3(3) = gamma(i)*(1+2*dx2)*(9-4*dx2**2)/16
		gamma3(4) = gamma(i)*(3+2*dx2)*(4*dx2**2-1)/48
		do j=col-1,col+2
			gamman(row-1,j) = gamman(row-1,j)+gamma3(j-col+2)*(3+2*dy2)*(4*dy2**2-1)/48
			gamman(row,j) = gamman(row,j)+gamma3(j-col+2)*(1+2*dy2)*(9-4*dy2**2)/16
			gamman(row+1,j) = gamman(row+1,j)+gamma3(j-col+2)*(1-2*dy2)*(9-4*dy2**2)/16
			gamman(row+2,j) = gamman(row+2,j)+gamma3(j-col+2)*(3-2*dy2)*(4*dy2**2-1)/48
		enddo

	enddo
	
	xc = xc*h
	yc = yc*h
	count1 = 1
    count2 = 1
	do i=0,ReGrid**2-ReGrid,ReGrid
		
		do j=1,ReGrid
			xp2(j+i) = gridx(j,count1)*h
			yp2(j+i) = gridy(j,count1)*h
			gamma2n(j+i) = gamman(j,count1)
            count2 = count2+1
		enddo
		count1 = count1 + 1
    enddo
    nv = count2-1
	!nv = count2-1    
 !   dist = sqrt((xc(2)-xc(1))**2+(yc(2)-yc(1))**2)/4
    xc2 = xc*(1+h/2.)
    yc2 = yc*(1+h/2.)
    countv=1
	maxxb = MAXVAL(xc2)
	minxb = MINVAL(xc2)
	maxyb = MAXVAL(yc2)
	minyb = MINVAL(yc2)
	do i=1,nv
		if (((xp2(i)-maxxb)*(xp2(i)-minxb)<0) .AND. ((yp2(i)-maxyb)*(yp2(i)-minyb)<0)) then
			count2 = 1
			do j=1,Panels
				if ((xp2(i)-xc2(j+1))*(xp2(i)-xc2(j))<0) then
					pan(count2) = j
					count2 = count2+1
				else if ((xp2(i)-xc2(j+1))==0) then
					pan(count2) = j
					count2 = count2+1						
				end if
			enddo
			ycheck(1) = (yc2(pan(1)+1)-yc2(pan(1)))/(xc2(pan(1)+1)-xc2(pan(1)))*(xp2(i)-xc2(pan(1)))+yc2(pan(1));
			ycheck(2) = (yc2(pan(2)+1)-yc2(pan(2)))/(xc2(pan(2)+1)-xc2(pan(2)))*(xp2(i)-xc2(pan(2)))+yc2(pan(2));
			if ((xc2(pan(1)+1)==xc2(pan(1))) .OR. (xc2(pan(2)+1)==xc2(pan(2)))) then
				ycheck(1) = yc2(pan(1));
				ycheck(2) = yc2(pan(2));
            end if
            if (ycheck(1)<ycheck(2)) then
                ycheck1 = ycheck(2)
                ycheck(2) = ycheck(1)
                ycheck(1) = ycheck1
			end if              
			if ((ycheck(1)>=yp2(i)) .AND. (ycheck(2)<=yp2(i))) then
                sgam = sgam+gamma2n(i)
			else if (gamma2n(i) /= 0.) then
				vortx(countv) = xp2(i)
				vorty(countv) = yp2(i)
				gamma(countv) = gamma2n(i)
				countv = countv + 1
			end if
		else if (gamma2n(i) /= 0.) then
			vortx(countv) = xp2(i)
			vorty(countv) = yp2(i)
			gamma(countv) = gamma2n(i)
			countv = countv + 1
		end if	
	enddo
	nv = countv-1    
    
    
	!sgam = 0.
	!maxx=MAXVAL(abs(gamma2n))
	!print *, "MAX Strength", maxx
	!count1 = 1
	!dist = sqrt((xc(2)-xc(1))**2+(yc(2)-yc(1))**2)/4
	!do i=1,ReGrid**2
	!	if ((abs(gamma2n(i))/=0.) .AND. (sqrt(xp2(i)**2+yp2(i)**2)>=0.5+dist)) then
	!		vortx(count1)=xp2(i)
	!		vorty(count1)=yp2(i)
	!		gamma(count1)=gamma2n(i)
	!		count1 = count1 + 1
	!	else
	!		sgam = sgam+gamma2n(i)
	!	end if
	!enddo
	!nv = count1-1
return
end subroutine Redistribute3