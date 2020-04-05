module MAIN_INIT  
	implicit none
	SAVE

    !--------- Integer Parameters ------
    integer, parameter :: Panels = 948!1496!948!1600!square=1160!
    integer, parameter :: mnv = 1000000
    integer, parameter :: maxgrid = 2500
    integer, parameter :: LWORKmax = Panels+Panels
    integer, parameter :: maxI = 5000
    
    !--------- Integers ----------------
    integer :: counter
    integer :: count2
    integer :: count3
    integer :: countv
    integer :: countP
    integer :: Flag
	integer :: i  
    integer :: INFO
    integer :: j
    integer :: j2
    integer :: k
    integer :: LWORK
    integer :: maxgc
    integer :: maxgr
    integer :: maxvxi
    integer :: minvxi
    integer :: maxvyi
    integer :: minvyi
    integer :: nb
    integer :: nbv    
    integer :: nv
    integer :: ReGrid   
    
    !--------- Integer Vectors----------
    integer :: index_vorts(mnv) 
    integer :: indexb(mnv)
    integer :: info2(Panels+1)
    integer :: ipiv(Panels+1)
    integer :: pan(2)
    integer :: points(mnv)
    integer :: points2(mnv)
    
    !--------- Reals Parameters --------    
    real, parameter :: pi = 3.14159265359
    real, parameter :: h = 0.005
    real, parameter :: T = 0.01
    real, parameter :: nu = 0.000001004
    
    !--------- Reals -------------------
    real :: Amp
    real :: area
    real :: cx
    real :: cy
    real :: Determine_Gamma
    real :: tau
    real :: theta
    real :: radius
    real :: mxg
    real :: mx
    real :: my
    real :: ddthetab    
    real :: check
    real :: maxxb
    real :: minxb
    real :: maxyb
    real :: minyb
    real :: maxvx
    real :: minvx
    real :: maxvy
    real :: minvy   
    real :: sgam
    real :: time
    real :: time2
    real :: mtheta
    real :: rad_C
    real :: vt_ie
    real :: ycheck1
    
    !--------- Reals Vectors -----------
    real :: DG(Panels+1)
    real :: Dt
    real :: dthetab(maxI) 
    real :: dxdt(5000*5000+3+1)
    real :: Fx(maxI)
    real :: Fy(maxI)
    real :: Fx2(Panels)
    real :: Fy2(Panels) 
    real :: Fx3(maxI)
    real :: Fy3(maxI)
    real :: Fx4(Panels)
    real :: Fy4(Panels)     
    real :: gamma(mnv)
    real :: gammast(mnv)
    real :: gx(maxgrid)
    real :: gy(maxgrid)
    real :: Ix(maxI)
    real :: Iy(maxI)
    real :: sigma(mnv)
    real :: sigma2(mnv)
    real :: thetab(maxI)
    real :: ti(4)
    real :: Uinf(2)
    real :: Vn(Panels)
    real :: vortx(mnv)
    real :: vortxp(mnv)
    real :: vorty(mnv)
    real :: vortyp(mnv)
    real :: Vt(Panels)
    real :: Vt2(Panels)      
    real :: vt_i(Panels+1)
    real :: vxst(mnv)
    real :: vyst(mnv) 
    real :: wi(4)
    real :: WORK(LWORKmax)
    real :: work2(2*Panels+2)
    real :: xc2(Panels+1) 
    real :: xc(Panels+1)
    real :: xci(Panels+1)
    real :: xpn(mnv)
    real :: xpred(mnv)
    real :: yc2(Panels+1)
    real :: yc(Panels+1) 
    real :: ycheck(2)
    real :: yci(Panels+1)
    real :: ypn(mnv)
    real :: ypred(mnv)
    
    !--------- Reals Matrix ------------
    real, dimension(mnv,13) :: box
    real, dimension(Panels+1,Panels) :: Bt
    real, dimension(Panels+1,Panels) :: Bt2
    real, dimension(Panels+1,Panels+1) ::   Bt_S
    real, dimension(Panels,Panels) ::   BtT
    real, dimension(Panels,Panels) ::   Bn
    real, dimension(Panels,Panels) ::   Bni
    real, dimension(100,2) :: erf
    real, dimension(maxgrid,maxgrid) :: gridI
    real, dimension(maxgrid,maxgrid) :: gridx
    real, dimension(maxgrid,maxgrid) :: gridy
    real, dimension(mnv,4) :: Rk4x
    real, dimension(mnv,4) :: Rk4y
    real, dimension(mnv,2) :: ux0
    real, dimension(mnv,2) :: ux02
    

    !--------- Complex Matrix ----------
    complex, dimension(mnv,10) :: alpha_box
    

   
end module MAIN_INIT 