        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:04 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PLACE_VORTS__genmod
          INTERFACE 
            SUBROUTINE PLACE_VORTS(VORTX,VORTY,XC2,YC2,H,NV,PANELS,XPN, &
     &YPN)
              INTEGER(KIND=4) :: PANELS
              INTEGER(KIND=4) :: NV
              REAL(KIND=4) :: VORTX(NV)
              REAL(KIND=4) :: VORTY(NV)
              REAL(KIND=4) :: XC2(PANELS+1)
              REAL(KIND=4) :: YC2(PANELS+1)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: XPN(NV)
              REAL(KIND=4) :: YPN(NV)
            END SUBROUTINE PLACE_VORTS
          END INTERFACE 
        END MODULE PLACE_VORTS__genmod
