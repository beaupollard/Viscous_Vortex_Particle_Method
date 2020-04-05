subroutine Grid_Create(nv,gamma,vortx,vorty,h,mx,my,ReGrid)
	implicit none
	integer :: nv, i, j, k, ReGrid
	real :: vortx(nv), vorty(nv), gamma(nv), minx, maxx, miny, maxy, h, my
	real :: sigy, sigx, dist, mingx, maxgx, mingy, maxgy, dx, dy, mx

	minx = 0
	maxx = 0
	miny = 0
	maxy = 0
	dist = 11
	
	do i=1,nv
		if (minx>vortx(i)) then
			minx = vortx(i)
		else if (maxx<vortx(i)) then
			maxx = vortx(i)
		end if
		if (miny>vorty(i)) then
			miny = vorty(i)
		else if (maxy<vorty(i)) then
			maxy = vorty(i)
		end if
	enddo
	mingx = nint(minx/h-dist)-1./2.
	maxgx = nint(maxx/h+dist)-1./2.
	dx = maxgx-mingx
	mingy = nint(miny/h-dist)-1./2.
	maxgy = nint(maxy/h+dist)-1./2.
	dy = maxgy-mingy
	if (dx > dy) then
		ReGrid = int(dx)
	else
		ReGrid = int(dy)
	end if
	mx = mingx
	my = maxgy
	
	!print *, ReGrid, mx, my
	
return
end subroutine Grid_Create