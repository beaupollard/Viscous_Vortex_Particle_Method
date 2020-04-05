subroutine PSE4(xc,yc,Panels,nv,gamma,vortx,vorty,sigma,nu,Dt,box,index_vorts,nb,nbv)
	implicit none
	integer :: Panels, i, k, nv, j, count2, index_vorts(nbv), nb, nbv, j2
	real :: xc(Panels+1), yc(Panels+1), pi, vortx(nv), vorty(nv), gamma(nv), inp, inp2
	real :: dist, xd2, yd2, dx, dy, dist2, Dt, con(nv), mxg, m3(nv), xci(2), yci(2), nci(2), hi, fk, fk2
	real :: limx, midx, midy, midx2, midy2, m, xd(nv), gammad(nv), yd(nv), dGdt, nu, sigma(nv), Sp!, DG2(nv)
	real, dimension(nb,12) :: box
	real :: DG2(nv)
	
	DG2 = 0.0
	!Sp = h*h 
	pi = 3.14159265359
	DG2 = 0.0
	limx = 10
	!do k=1,nb
	!	print *, box(k,:)
	!end do 
	!call Print_Data(box(1:nb,:),nb,12,"box2.txt",8,10)
    !!$OMP PARALLEL DO PRIVATE(midx,midy,dx,dy,dist,dist2,xci,yci,nci,m,xd2,yd2)
	!do k=1,nv
	!	dx = 0
	!	dy = 0
	!	midx = 0
	!	midy = 0
	!	dist = 100
 !       
	!	if (sqrt(vortx(k)**2+vorty(k)**2)<0.85+sigma(k)*3) then
	!		do j=1,Panels
	!			midx = (xc(j+1)+xc(j))/2
	!			midy = (yc(j+1)+yc(j))/2
	!			dist2 = sqrt((vortx(k)-midx)**2+(vorty(k)-midy)**2)
	!			if (dist>=dist2) then
	!				dist = dist2
	!				dx = xc(j+1)-xc(j)
	!				dy = yc(j+1)-yc(j)
	!				xci(1) = xc(j)
	!				xci(2) = xc(j+1)
	!				yci(1) = yc(j)
	!				yci(2) = yc(j+1)
	!				nci(1) = dy/sqrt(dx**2+dy**2)
	!				nci(2) = -dx/sqrt(dx**2+dy**2)
	!			end if
	!		enddo
	!		m = (yci(2)-yci(1))/(xci(2)-xci(1))
	!		xd2 = 1./(m+1./m)*(m*xci(1)-yci(1)+vorty(k)+1./m*vortx(k))
	!		yd2 = m*(xd2-xci(1))+yci(1)
	!		dist2 = sqrt((xd2-vortx(k))**2+(yd2-vorty(k))**2)	
	!		if (xci(2)==xci(1)) then
	!			yd(k) = vorty(k)
	!			xd(k) = xci(1) - vortx(k)/abs(vortx(k))*dist
	!		else if (yci(2)==yci(1)) then
	!			xd(k) = vortx(k)
	!			yd(k) = yci(1)-vorty(k)/abs(vorty(k))*dist
	!		else
	!			xd(k) = xd2-dist2*nci(1)
	!			yd(k) = yd2-dist2*nci(2)
	!			!gammad(k) = gamma(k)
	!		end if
	!		con(k) = 1
	!	else
	!		con(k) = 0
	!		xd(k) = -1000
	!		yd(k) = -1000
	!		gammad(k) = 0
	!	end if
 !
 !   enddo
    !!$OMP END PARALLEL DO
	do k=1,nv
		dx = 0
		dy = 0
		midx = 0
		midy = 0
		dist = 100
		do j=1,Panels
			midx = (xc(j+1)+xc(j))/2
			midy = (yc(j+1)+yc(j))/2
			dist2 = sqrt((vortx(k)-midx)**2+(vorty(k)-midy)**2)
			if (dist>=dist2) then
				dist = dist2
				dx = xc(j+1)-xc(j)
				dy = yc(j+1)-yc(j)
				xci(1) = xc(j)
				xci(2) = xc(j+1)
				yci(1) = yc(j)
				yci(2) = yc(j+1)
				nci(1) = dy/sqrt(dx**2+dy**2)
				nci(2) = -dx/sqrt(dx**2+dy**2)
			end if
		enddo        
		if (dist<sigma(k)*5.) then

			m = (yci(2)-yci(1))/(xci(2)-xci(1))
			xd2 = 1./(m+1./m)*(m*xci(1)-yci(1)+vorty(k)+1./m*vortx(k))
			yd2 = m*(xd2-xci(1))+yci(1)
			dist2 = sqrt((xd2-vortx(k))**2+(yd2-vorty(k))**2)	
			if (xci(2)==xci(1)) then
				yd(k) = vorty(k)
				xd(k) = xci(1) - vortx(k)/abs(vortx(k))*dist
			else if (yci(2)==yci(1)) then
				xd(k) = vortx(k)
				yd(k) = yci(1)-vorty(k)/abs(vorty(k))*dist
			else
				xd(k) = xd2-dist2*nci(1)
				yd(k) = yd2-dist2*nci(2)
				!gammad(k) = gamma(k)
			end if
			con(k) = 1
		else
			con(k) = 0
			xd(k) = 0
			yd(k) = 0
			gammad(k) = 0
		end if

    enddo    
    
	!call Print_Data(box(1:nb,:),nb,12,"box.txt",7,7)
	!do i=1,nbv
	!	print *, index_vorts(i)
	!end do
!!--------- WHY are there zeros in this matrix -------------
	do i=1,nbv
		midx = (box(index_vorts(i),1)+box(index_vorts(i),2))/2
		midy = (box(index_vorts(i),3)+box(index_vorts(i),4))/2		
		do k=1,nbv
			midx2 = (box(index_vorts(k),2)+box(index_vorts(k),1))/2
			midy2 = (box(index_vorts(k),4)+box(index_vorts(k),3))/2
			dx = midx-midx2
			dy = midy-midy2
			dist = sqrt(dx**2+dy**2)
			
			if (dist < 1.5*(box(index_vorts(i),2)-box(index_vorts(i),1))) then
				do j=int(box(index_vorts(i),5)),int(box(index_vorts(i),6))
					dGdt = 0
					do j2=int(box(index_vorts(k),5)),int(box(index_vorts(k),6))
						!print *, j, j2, (box(index_vorts(i),5))
						if ((j/=0) .AND. (j2/=0)) then
							dx=vortx(j)-vortx(j2)
							dy=vorty(j)-vorty(j2)
							dist = sqrt(dx**2+dy**2)
							if (dist < sigma(j2)*10) then
                                Sp = ((sigma(j)+sigma(j2))/2)**2
								dx=vortx(j)-xd(j2)
								dy=vorty(j)-yd(j2)						
								dist2 = sqrt(dx**2+dy**2)
                                hi = (sigma(j)**2+sigma(j2)**2)/2
								inp = 1/(2*pi*hi)*exp(-(dist*dist/(hi))/2)
								inp2 = 1/(2*pi*hi)*exp(-(dist2*dist2/(hi))/2)
								dGdt = dGdt+2*nu/(hi)*((sigma(j)**2)*gamma(j2)-(sigma(j2)**2)*gamma(j))*(inp+inp2*con(j2))
                            endif
						else
							dGdt = dGdt
						end if
					end do
					!print *, nbv, j, k
					DG2(j) = DG2(j)+dGdt
				end do
			end if
		enddo
		
	end do    
	!do i=1,nbv
 !       if (int(box(i,5)) /=0) then
	!	    midx = (box(i,1)+box(i,2))/2
	!	    midy = (box(i,3)+box(i,4))/2		
	!	    do k=1,nbv
 !               if (int(box(k,5)) /=0) then
	!		        midx2 = (box(k,2)+box(k,1))/2
	!		        midy2 = (box(k,4)+box(k,3))/2
	!		        dx = midx-midx2
	!		        dy = midy-midy2
	!		        dist = sqrt(dx**2+dy**2)
	!		        fk2 = sqrt((box(k,2)-box(k,1))**2+(box(k,3)-box(k,4))**2)/2.
 !                    fk = sqrt((box(i,2)-box(i,1))**2+(box(i,3)-box(i,4))**2)/1.9
	!		        if (dist < fk2+fk) then
	!			        do j=int(box(i,5)),int(box(i,6))
	!				        dGdt = 0
	!				        do j2=int(box(k,5)),int(box(k,6))
	!					        !print *, j, j2, (box(index_vorts(i),5))
	!					        if (j /= j2) then
	!						        dx=vortx(j)-vortx(j2)
	!						        dy=vorty(j)-vorty(j2)
	!						        dist = sqrt(dx**2+dy**2)
	!						        if (dist < sigma(j2)*10) then
 !                                       Sp = ((sigma(j)+sigma(j2))/2)**2
	!							        dx=vortx(j)-xd(j2)
	!							        dy=vorty(j)-yd(j2)						
	!							        dist2 = sqrt(dx**2+dy**2)
 !                                       hi = (sigma(j)**2+sigma(j2)**2)/2
	!							        inp = 1/(2*pi*hi)*exp(-(dist*dist/(hi))/2)
	!							        inp2 = 1/(2*pi*hi)*exp(-(dist2*dist2/(hi))/2)
	!							        dGdt = dGdt+2*nu/(hi)*((sigma(j)**2)*gamma(j2)-(sigma(j2)**2)*gamma(j))*(inp+inp2*con(j2))
	!						        end if
	!					        else
	!						        dGdt = dGdt
	!					        end if
	!				        end do
	!				        !print *, nbv, j, k
	!				        DG2(j) = DG2(j)+dGdt
	!			        end do
 !                   end if
 !               end if
	!	    enddo
 !       end if
 !       
	!end do

	do i=1,nv
		dGdt=0
		!do j=1,nbv
		dGdt = dGdt+DG2(i)
		!end do
		gamma(i) = gamma(i) + dGdt*Dt
	enddo
	count2 = 1
	mxg =  MAXVAL(abs(gamma(1:nv)))
	!call Print_Data(DG2,nv,nbv,"DG2.txt",7,10)
	! ! do i=1,nv	
		! ! if (abs(gamma(i))>mxg*0.00001)then
			! ! xd(count2) = vortx(i)
			! ! yd(count2) = vorty(i)
			! ! gammad(count2) = gamma(i)
			! ! count2 = count2+1
		! ! end if
	! ! enddo
	! ! nv = count2-1
	! ! !!vortx = xd
	! ! !!vorty = yd
	! ! !!gamma = gammad
	! ! vortx(1:count2-1) = xd(1:count2-1)
	! ! vorty(1:count2-1) = yd(1:count2-1)
	! ! gamma(1:count2-1) = gammad(1:count2-1)
	!nv = count2-1
	!!print *, nv
	
	
	
	return
end subroutine PSE4