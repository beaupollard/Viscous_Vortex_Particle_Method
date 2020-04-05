subroutine Grid_Create2(xc,yc,gridx,gridy,gridI,Panels,maxgrid,h,gx,gy)
	implicit none
	integer :: Panels, maxgrid, i, j
	real :: xc(Panels+1), yc(Panels+1), maxxb, minxb, minyb, maxyb, h, chord
    real, dimension(maxgrid,maxgrid) :: gridx, gridy, gridI
    real :: gx(maxgrid), gy(maxgrid), giy(maxgrid), gix(maxgrid), BL, conv

	maxxb = MAXVAL(xc)
	minxb = MINVAL(xc)
	maxyb = MAXVAL(yc)
	minyb = MINVAL(yc)
    conv = 5.
    BL = 1.25
    gix = h
    giy = h
    gy=0.
    gx=0.
    gridx=0.
    gridy=0.
    if (maxxb-minxb .GE. maxyb-minyb) then
        chord = maxxb-minxb
    else
        chord = maxyb-minyb
    end if

    
    ! Make the y grid fit in the middle
    !gy(nint(maxgrid/2)) = 0.
    gx(1) = minxb-h*25.1
    gy((maxgrid/2)) = h*0.01
    do i=1,(maxgrid/2)-1
        if (abs(gy((maxgrid/2)-i+1)) .GE. BL*chord) then
            gy((maxgrid/2)-i) = gy((maxgrid/2)-i+1)+abs((conv-((conv-1.)*BL**2)/(gy((maxgrid/2)-i+1))/BL)*h)
            gy((maxgrid/2)+i) = -gy((maxgrid/2)-i)+h*0.01!gy((maxgrid/2)+i-1)-abs((4.-(3.*1.5**2)/(gy((maxgrid/2)+i-1))/1.5)*h)
            !gy((maxgrid/2)-i) = gy((maxgrid/2)-i+1)+abs((4.-(3.*1.5**2)/(gy((maxgrid/2)-i+1))/1.5)*h)
            !print *, gy((maxgrid/2)+i-1), gy((maxgrid/2)+i), gy((maxgrid/2)+i+1)
            !print *, (4.-(3.*1.5**2)/(gy((maxgrid/2)+i-1))/1.5)
            !print *, (4.-(3.*1.5**2)/(gy((maxgrid/2)-i+1))/1.5)
            giy((maxgrid/2)+i) = abs((conv-((conv-1.)*BL**2)/(gy((maxgrid/2)-i+1))/BL)*h)!abs((4.-(3.*1.5**2)/(gy((maxgrid/2)+i-1))/1.5)*h)
            giy((maxgrid/2)-i) = abs((conv-((conv-1.)*BL**2)/(gy((maxgrid/2)-i+1))/BL)*h)
        else
            giy((maxgrid/2)+i) = h
            giy((maxgrid/2)-i) = h
            gy((maxgrid/2)+i) = gy((maxgrid/2)+i-1)-h
            gy((maxgrid/2)-i) = gy((maxgrid/2)-i+1)+h
        endif
    enddo
    gy(maxgrid) = gy(maxgrid-1)-3.*h
    giy(maxgrid) = giy(maxgrid-1)
    do i=2,maxgrid
        if (abs(gx(i-1)) .GE. BL*chord) then
            gx(i) = gx(i-1)+(conv-((conv-1.)*BL**2)/(gx(i-1))/BL)*h
            gix(i) = (conv-((conv-1.)*BL**2)/(gx(i-1))/BL)*h
        !elseif (abs(gx(i-1)) .GE. 3*chord) then
        !    gx(i) = gx(i-1)+4*h
        else
            gix(i) = h
            gx(i) = gx(i-1)+h
        endif
    enddo    
    !do i=1,(maxgrid/2)-1
    !    if (abs(gy((maxgrid/2)-i+1)) .GE. 1.5*chord) then
    !        gy((maxgrid/2)+i) = gy((maxgrid/2)+i-1)-2.*h
    !        gy((maxgrid/2)-i) = gy((maxgrid/2)-i+1)+2.*h
    !        giy(i) = 2.*h
    !    else
    !        giy(i) = h
    !        gy((maxgrid/2)+i) = gy((maxgrid/2)+i-1)-h
    !        gy((maxgrid/2)-i) = gy((maxgrid/2)-i+1)+h
    !    endif
    !enddo
    !gy(maxgrid) = gy(maxgrid-1)-2.*h
    !giy(maxgrid) = 2.*h
    !do i=2,maxgrid
    !    if (abs(gx(i-1)) .GE. 1.5*chord) then
    !        gx(i) = gx(i-1)+2.*h
    !        gix(i) = 2.*h
    !    !elseif (abs(gx(i-1)) .GE. 3*chord) then
    !    !    gx(i) = gx(i-1)+4*h
    !    else
    !        gix(i) = h
    !        gx(i) = gx(i-1)+h
    !    endif
    !enddo
    
        
        
	do i=1,maxgrid
		do j=1,maxgrid
	        gridy(j,i) = gy(j)
	        gridx(j,i) = gx(i)
            gridI(j,i) = max(gix(i),giy(j))
        enddo
    enddo
    

	
    return
end subroutine Grid_Create2