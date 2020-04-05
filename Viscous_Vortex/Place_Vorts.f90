subroutine Place_Vorts(vortx,vorty,xc2,yc2,h,nv,Panels,xpn,ypn)
	implicit none
    integer :: nv, Panels, lengy, lengx, i, j, k, pan(2), countv, count2
	real :: vortx(nv), vorty(nv), xc2(Panels+1), yc2(Panels+1), h
	real :: minxb, maxxb, maxyb, minyb, ycheck(2), xpn(nv), ypn(nv), ycheck1
	
	
	maxxb = MAXVAL(xc2)
	minxb = MINVAL(xc2)
	maxyb = MAXVAL(yc2)
	minyb = MINVAL(yc2)
	lengx = int((maxxb-minxb)/h)+22
	lengy = int((maxyb-minyb)/h)+22
	
	countv = 1
	do i=1,lengx
		do j=1,lengy
			vortx((i-1)*lengy+j) = minxb-11*h+h*i
			vorty((i-1)*lengy+j) = maxyb+11*h-h*j
		enddo
	enddo
	nv = lengx*lengy
    xpn = vortx
    ypn = vorty
        !    call Print_Data(vortx,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vortx11.dat",44,61,0)
		!call Print_Data(vorty,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/vorty11.dat",44,62,0)
		!call Print_Data(gamma,nv,1,"D:/bpoll/Fortran/Large_Data/Foil/gamma10.dat",44,63,0)
    
	do i=1,nv
        !if (i == 40820) then
        !    print *, "Stop", xpn(i), ypn(i), (xpn(i)-minxb), (xpn(i)-maxxb), (ypn(i)-maxyb), (ypn(i)-minyb)
        !    
        !end if        
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
				ycheck(1) = yc2(pan(1))
				ycheck(2) = yc2(pan(2))
            end if
            
            if (ycheck(1)<ycheck(2)) then
                ycheck1 = ycheck(2)
                ycheck(2) = ycheck(1)
                ycheck(1) = ycheck1
			end if
			if ((ycheck(1)>=ypn(i)) .AND. (ycheck(2)<=ypn(i))) then
			else
				vortx(countv) = xpn(i)
				vorty(countv) = ypn(i)
				countv = countv + 1
			end if
		else
			vortx(countv) = xpn(i)
			vorty(countv) = ypn(i)
			countv = countv + 1
		end if	
	enddo
	nv = countv-1
    
	!print *, nv, vortx(nv), vorty(nv), lengx, lengy
	!do i=1,nv
	!	if (((vortx(i)-maxxb)*(vortx(i)-minxb)<0) .AND. ((vorty(i)-maxyb)*(vorty(i)-minyb)<0)) then
	!		
	!		count2 = 1
	!		do j=1,Panels
	!			if ((vortx(i)-xc2(j+1))*(vortx(i)-xc2(j))<0) then
	!				
	!				pan(count2) = j
	!				count2 = count2+1
	!			else if ((vortx(i)-xc2(j+1))==0) then
	!				pan(count2) = j
	!				count2 = count2+1
	!				
	!			end if
	!		enddo
	!		ycheck(1) = (yc2(pan(1)+1)-yc2(pan(1)))/(xc2(pan(1)+1)-xc2(pan(1)))*(vortx(i)-xc2(pan(1)))+yc2(pan(1));
	!		ycheck(2) = (yc2(pan(2)+1)-yc2(pan(2)))/(xc2(pan(2)+1)-xc2(pan(2)))*(vortx(i)-xc2(pan(2)))+yc2(pan(2));
	!		if ((xc2(pan(1)+1)==xc2(pan(1))) .OR. (xc2(pan(2)+1)==xc2(pan(2)))) then
	!			ycheck(1) = yc2(pan(1));
	!			ycheck(2) = yc2(pan(2));
	!		end if
	!		if ((ycheck(1)>=vorty(i)) .AND. (ycheck(2)<=vorty(i))) then
	!			! print *, i
	!		else
	!			vortx(countv) = vortx(i)
	!			vorty(countv) = vorty(i)
	!			countv = countv + 1
	!		end if
	!	else
	!		vortx(countv) = vortx(i)
	!		vorty(countv) = vorty(i)
	!		countv = countv + 1
	!	end if	
	!enddo
	!nv = countv-1
	print *, nv
return
end subroutine Place_Vorts