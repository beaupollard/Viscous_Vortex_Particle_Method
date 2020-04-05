subroutine Rotate_Body(xc,yc,angle,Panels,xc2,yc2,h)
	implicit none
	integer :: Panels, i
	real :: xc(Panels+1), yc(Panels+1), angle, xc2(Panels+1), yc2(Panels+1)
	real :: dist, h
	!print *, "Angle", angle
	xc2 = xc
	yc2 = yc
	! do i=1,Panels+1
		! print *, xc(i), yc(i), xc2(i), yc2(i)
	! enddo		
	do i=1,Panels+1
		xc(i) = xc2(i)*cos(angle)-yc2(i)*sin(angle)
		yc(i) = xc2(i)*sin(angle)+yc2(i)*cos(angle)
		! print *, angle, xc2(i)*cos(angle)-yc2(i)*sin(angle), yc2(i)*sin(angle)+xc2(i)*cos(angle)
	enddo
	dist = sqrt((xc(2)-xc(1))**2+(yc(2)-yc(1))**2)
	xc2 = xc*(1.+1.*h/4.)
	yc2 = yc*(1.+1.*h/4.)
return 
end subroutine Rotate_Body