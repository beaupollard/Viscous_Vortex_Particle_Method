        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:05 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ROTATE_BODY__genmod
          INTERFACE 
            SUBROUTINE ROTATE_BODY(XC,YC,ANGLE,PANELS,XC2,YC2,H)
              INTEGER(KIND=4) :: PANELS
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: ANGLE
              REAL(KIND=4) :: XC2(PANELS+1)
              REAL(KIND=4) :: YC2(PANELS+1)
              REAL(KIND=4) :: H
            END SUBROUTINE ROTATE_BODY
          END INTERFACE 
        END MODULE ROTATE_BODY__genmod
