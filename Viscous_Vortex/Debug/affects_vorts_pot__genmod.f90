        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:03 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AFFECTS_VORTS_POT__genmod
          INTERFACE 
            SUBROUTINE AFFECTS_VORTS_POT(VORTX,VORTY,GAMMA,NV,XC,YC,    &
     &PANELS,VT,UINF,SGAM,VN,DDTHETAB,DTHETAB,VT_IE,FLAG)
              INTEGER(KIND=4) :: PANELS
              INTEGER(KIND=4) :: NV
              REAL(KIND=4) :: VORTX(NV)
              REAL(KIND=4) :: VORTY(NV)
              REAL(KIND=4) :: GAMMA(NV)
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: VT(PANELS)
              REAL(KIND=4) :: UINF(2)
              REAL(KIND=4) :: SGAM
              REAL(KIND=4) :: VN(PANELS)
              REAL(KIND=4) :: DDTHETAB
              REAL(KIND=4) :: DTHETAB
              REAL(KIND=4) :: VT_IE
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE AFFECTS_VORTS_POT
          END INTERFACE 
        END MODULE AFFECTS_VORTS_POT__genmod
