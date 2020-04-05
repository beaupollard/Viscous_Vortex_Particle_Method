        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:04 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FMM2__genmod
          INTERFACE 
            SUBROUTINE FMM2(VORTX,VORTY,GAMMA,NV,BOX,NB,SIGMA,UX0,UINF, &
     &RK2,DT,NBV,INDEX_VORTS,XPN,YPN,UX02,VORTX2,VORTY2,ALPHA,POINTS,   &
     &POINTS2,VX2,VY2,GAMMA2,SIGMA2,INDEXB,PANELS,BN,XC,YC,DTHETAB)
              INTEGER(KIND=4) :: PANELS
              INTEGER(KIND=4) :: NB
              INTEGER(KIND=4) :: NV
              REAL(KIND=4) :: VORTX(NV)
              REAL(KIND=4) :: VORTY(NV)
              REAL(KIND=4) :: GAMMA(NV)
              REAL(KIND=4) :: BOX(NB,13)
              REAL(KIND=4) :: SIGMA(NV)
              REAL(KIND=4) :: UX0(NV,2)
              REAL(KIND=4) :: UINF(2)
              INTEGER(KIND=4) :: RK2
              REAL(KIND=4) :: DT
              INTEGER(KIND=4) :: NBV
              INTEGER(KIND=4) :: INDEX_VORTS(NV)
              REAL(KIND=4) :: XPN(NV)
              REAL(KIND=4) :: YPN(NV)
              REAL(KIND=4) :: UX02(NV,2)
              REAL(KIND=4) :: VORTX2(NV)
              REAL(KIND=4) :: VORTY2(NV)
              COMPLEX(KIND=4) :: ALPHA(NB,10)
              INTEGER(KIND=4) :: POINTS(20000)
              INTEGER(KIND=4) :: POINTS2(20000)
              REAL(KIND=4) :: VX2(NV)
              REAL(KIND=4) :: VY2(NV)
              REAL(KIND=4) :: GAMMA2(NV)
              REAL(KIND=4) :: SIGMA2(NV)
              INTEGER(KIND=4) :: INDEXB(NB)
              REAL(KIND=4) :: BN(PANELS,PANELS)
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: DTHETAB
            END SUBROUTINE FMM2
          END INTERFACE 
        END MODULE FMM2__genmod
