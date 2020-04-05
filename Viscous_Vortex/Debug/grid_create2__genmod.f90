        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:05 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GRID_CREATE2__genmod
          INTERFACE 
            SUBROUTINE GRID_CREATE2(XC,YC,GRIDX,GRIDY,GRIDI,PANELS,     &
     &MAXGRID,H,GX,GY)
              INTEGER(KIND=4) :: MAXGRID
              INTEGER(KIND=4) :: PANELS
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: GRIDX(MAXGRID,MAXGRID)
              REAL(KIND=4) :: GRIDY(MAXGRID,MAXGRID)
              REAL(KIND=4) :: GRIDI(MAXGRID,MAXGRID)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: GX(MAXGRID)
              REAL(KIND=4) :: GY(MAXGRID)
            END SUBROUTINE GRID_CREATE2
          END INTERFACE 
        END MODULE GRID_CREATE2__genmod
