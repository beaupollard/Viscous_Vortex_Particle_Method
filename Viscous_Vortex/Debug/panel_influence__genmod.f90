        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:05 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PANEL_INFLUENCE__genmod
          INTERFACE 
            SUBROUTINE PANEL_INFLUENCE(XC,YC,PANELS,BT,BN)
              INTEGER(KIND=4) :: PANELS
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: BT(PANELS+1,PANELS)
              REAL(KIND=4) :: BN(PANELS,PANELS)
            END SUBROUTINE PANEL_INFLUENCE
          END INTERFACE 
        END MODULE PANEL_INFLUENCE__genmod
