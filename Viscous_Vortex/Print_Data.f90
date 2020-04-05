subroutine Print_Data(input,r,c,title,leng,un,flag)
	implicit none
	integer :: leng, un, flag
	character(len = leng) :: title
	integer :: r, c, i, j
	real, dimension(r,c) :: input
	
	if (flag==1) then
		open(unit = un,file = title,position='append')
	else
		open(unit = un,file = title)
	end if
	!open(unit = un,file = title,position='append')
	!print *, 0
	do i=1,r
		!print *, i
		!do j=1,c
			write(un,*) input(i,:)
		!enddo
	enddo
	if (flag==1) then
		write(un,*) -100000
	end if
	close(un)

return
end subroutine Print_Data

