subroutine Remove_Vorts(Panels,nv,xc2,yc2,xpn,ypn,gamma,vortx,vorty,sgam,ux0,nu,lim,flag)
    implicit none
    integer :: Panels, nv, i, j, pan(2), count2, countv, count3, flag
    real :: xc2(Panels+1), yc2(Panels+1), maxxb, maxyb, minxb, minyb, xpn(nv), ypn(nv), gamma(nv), vortx(nv), vorty(nv)
    real :: sgam, ycheck(2), nu, lim
    real, dimension(nv,2) :: ux0
    countv = 1	
	maxxb = MAXVAL(xc2)
	minxb = MINVAL(xc2)
	maxyb = MAXVAL(yc2)
	minyb = MINVAL(yc2)
    if (flag==1) then
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
			    if ((ycheck(1)>=ypn(i)) .AND. (ycheck(2)<=ypn(i))) then
                    sgam = sgam+gamma(i)
			    else if (abs(gamma(i))/nu>=lim) then
				    vortx(countv) = xpn(i)
				    vorty(countv) = ypn(i)
				    gamma(countv) = gamma(i)
				    ux0(countv,:) = ux0(i,:)
				    countv = countv + 1
                else
                    sgam = sgam+gamma(i)
                    count3 = count3 + 1
			    end if
		    else if (abs(gamma(i))/nu>=lim) then
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
    else
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
			    if ((ycheck(1)>=ypn(i)) .AND. (ycheck(2)<=ypn(i))) then
                    sgam = sgam+gamma(i)
			    else
				    vortx(countv) = xpn(i)
				    vorty(countv) = ypn(i)
				    gamma(countv) = gamma(i)
				    ux0(countv,:) = ux0(i,:)
				    countv = countv + 1
			    end if
		    else
			    vortx(countv) = xpn(i)
			    vorty(countv) = ypn(i)
			    gamma(countv) = gamma(i)
			    ux0(countv,:) = ux0(i,:)
			    countv = countv + 1
		    end if	
	    enddo
	    nv = countv-1        
    end if
    

    return
end subroutine Remove_Vorts