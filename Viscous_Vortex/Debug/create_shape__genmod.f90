        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:06 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CREATE_SHAPE__genmod
          INTERFACE 
            SUBROUTINE CREATE_SHAPE(XC,YC,PANELS,RADIUS,VORTX,VORTY,H,  &
     &GAMMA,MNV)
              INTEGER(KIND=4) :: MNV
              INTEGER(KIND=4) :: PANELS
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: RADIUS
              REAL(KIND=4) :: VORTX(MNV)
              REAL(KIND=4) :: VORTY(MNV)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: GAMMA(MNV)
            END SUBROUTINE CREATE_SHAPE
          END INTERFACE 
        END MODULE CREATE_SHAPE__genmod
