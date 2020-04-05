subroutine Print_Mat(input,r,c,title,leng,un,flag)
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
    write(un,*) r
    write(un,*) c
	do i=1,c
		!print *, i
		do j=1,r
			write(un,*) input(j,i)
        enddo
       !write(un,*) "\n"
	enddo

	close(un)

return
end subroutine Print_Mat
