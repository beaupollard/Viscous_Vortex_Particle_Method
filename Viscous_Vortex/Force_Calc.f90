subroutine Force_Calc(vortx,vorty,gamma,xc,yc,DG,nv,Panels,Fx,Fy,Fx2,Fy2,sigma,Fx4,Fy4,Fx3,Fy3,nu,Dt,area,ddthetab,Uinf)
	implicit none
    integer :: Panels, nv, i, j, k
    real :: gamma(nv), vortx(nv), vorty(nv), xc(Panels+1), yc(Panels+1), DG(Panels+1), Fx, Fy
    real :: ti(4), wi(4), pi, xp, yp, distL, csf, ssf, dx, dy, Fxi, Fyi, Fx2(Panels), Fy2(Panels), w_b, ddthetab, xp3, yp3
    real :: sigma(nv), Fxi2, Fyi2, Fx4(Panels), Fy4(Panels), Fx3, Fy3, nu, Dt, area, hy, psi(3), dist, xp2, yp2, Uinf(2)
    
  	wi(1) = (18.0-sqrt(30.0))/36.0
	wi(2) = (18.0+sqrt(30.0))/36.0
	wi(3) = (18.0+sqrt(30.0))/36.0
	wi(4) = (18.0-sqrt(30.0))/36.0
	ti(1) = -sqrt(3.0/7.0+2.0/7.0*sqrt(6.0/5.0))
	ti(2) = -sqrt(3.0/7.0-2.0/7.0*sqrt(6.0/5.0))
	ti(3) = sqrt(3.0/7.0-2.0/7.0*sqrt(6.0/5.0))
	ti(4) = sqrt(3.0/7.0+2.0/7.0*sqrt(6.0/5.0))
	pi = 3.14159265359
    hy = sigma(1)*1.2
    Fx = 0.
    Fy = 0.
    Fxi = 0.
    Fyi = 0.
  !  !!$OMP PARALLEL DO PRIVATE(dx,dy,distL,csf,ssf,xp,yp,Fxi,Fyi,Fxi2,Fyi2)
  !  do i=1,Panels
  !      dx = xc(i+1)-xc(i)
		!dy = yc(i+1)-yc(i)
		!distL = sqrt(dx*dx+dy*dy)
		!csf = dx/distL
		!ssf = dy/distL
  !      Fxi = 0.
  !      Fyi = 0.
  !      Fxi2 = 0.
  !      Fyi2 = 0.        
  !      do k=1,nv
  !          Fxi = Fxi + csf*gamma(k)/sigma(k)
  !          Fyi = Fyi + ssf*gamma(k)/sigma(k)
  !          Fxi2 = Fxi2 + csf*gamma(k)
  !          Fyi2 = Fyi2 + ssf*gamma(k)      
  !      end do
  !      Fx2(i) = Fxi2
  !      Fy2(i) = Fyi2
  !      Fx4(i) = Fxi
  !      Fy4(i) = Fyi        
  !  end do
  !  !!$OMP END PARALLEL DO
    Fx = 0.
    Fy = 0.
    Fx3 = 0.
    Fy3 = 0.    
    do i=1,Panels
        dx = xc(i+1)-xc(i)
		dy = yc(i+1)-yc(i)
		distL = sqrt(dx*dx+dy*dy)
		csf = dx/distL
		ssf = dy/distL
        do j=1,4
            xp = (xc(i+1)+xc(i))/2. + ti(j)*distL*csf/2.
            yp = (yc(i+1)+yc(i))/2. + ti(j)*distL*ssf/2.
            psi = 0.
            do k=1,nv
                dx = xp-vortx(k)
                dy = yp-vorty(k)
                dist = sqrt(dx**2+dy**2)
                psi(1) = psi(1)-1./(2.*pi)*gamma(k)*LOG(dist)
                xp2 = xp + hy*ssf
                yp2 = yp - hy*csf
                dx = xp2-vortx(k)
                dy = yp2-vorty(k)
                dist = sqrt(dx**2+dy**2)
                psi(2) = psi(2)-1./(2.*pi)*gamma(k)*LOG(dist)
                xp3 = xp + 2.*hy*ssf
                yp3 = yp - 2.*hy*csf
                dx = xp3-vortx(k)
                dy = yp3-vorty(k)
                dist = sqrt(dx**2+dy**2)
                psi(3) = psi(3)-1./(2.*pi)*gamma(k)*LOG(dist)
            end do
            psi(1) = psi(1) - ddthetab*LOG(sqrt(xp**2+yp**2))+Uinf(1)*yp-Uinf(2)*xp
            psi(2) = psi(2) - ddthetab*LOG(sqrt(xp2**2+yp2**2))+Uinf(1)*yp2-Uinf(2)*xp2
            psi(3) = psi(3) - ddthetab*LOG(sqrt(xp3**2+yp3**2))+Uinf(1)*yp3-Uinf(2)*xp3
            w_b = (7*psi(1)-8*psi(2)+psi(3))/(2.*hy**2)
            Fx = Fx + wi(j)*(csf*w_b + (yp*DG(i)/(-nu*Dt)))
            Fy = Fy + wi(j)*(ssf*w_b - (xp*DG(i)/(-nu*Dt)))
            Fx3 = Fx3 + wi(j)*(csf*w_b + (yp*DG(i)/(-nu*Dt)))
            Fy3 = Fy3 + wi(j)*(ssf*w_b - (xp*DG(i)/(-nu*Dt))) 
            Fx2(i) = Fx
            Fy2(i) = Fy
        end do
    end do
    
    Fx3 = Fx3*nu*997
    Fx = Fx*nu*997
    Fy3 = Fy3*nu*997
    Fy = Fy*nu*997
    return
end subroutine Force_Calc    