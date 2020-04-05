        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:06 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FORCE_CALC__genmod
          INTERFACE 
            SUBROUTINE FORCE_CALC(VORTX,VORTY,GAMMA,XC,YC,DG,NV,PANELS, &
     &FX,FY,FX2,FY2,SIGMA,FX4,FY4,FX3,FY3,NU,DT,AREA,DDTHETAB,UINF)
              INTEGER(KIND=4) :: PANELS
              INTEGER(KIND=4) :: NV
              REAL(KIND=4) :: VORTX(NV)
              REAL(KIND=4) :: VORTY(NV)
              REAL(KIND=4) :: GAMMA(NV)
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: DG(PANELS+1)
              REAL(KIND=4) :: FX
              REAL(KIND=4) :: FY
              REAL(KIND=4) :: FX2(PANELS)
              REAL(KIND=4) :: FY2(PANELS)
              REAL(KIND=4) :: SIGMA(NV)
              REAL(KIND=4) :: FX4(PANELS)
              REAL(KIND=4) :: FY4(PANELS)
              REAL(KIND=4) :: FX3
              REAL(KIND=4) :: FY3
              REAL(KIND=4) :: NU
              REAL(KIND=4) :: DT
              REAL(KIND=4) :: AREA
              REAL(KIND=4) :: DDTHETAB
              REAL(KIND=4) :: UINF(2)
            END SUBROUTINE FORCE_CALC
          END INTERFACE 
        END MODULE FORCE_CALC__genmod
