        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:04 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DETERMINE_ERF__genmod
          INTERFACE 
            SUBROUTINE DETERMINE_ERF(XC,YC,PANELS,VORTX,VORTY,H,GAMMA,NV&
     &,ERF,VT,NU,DT)
              INTEGER(KIND=4) :: NV
              INTEGER(KIND=4) :: PANELS
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: VORTX(NV)
              REAL(KIND=4) :: VORTY(NV)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: GAMMA(NV)
              REAL(KIND=4) :: ERF(100,2)
              REAL(KIND=4) :: VT(PANELS+1)
              REAL(KIND=4) :: NU
              REAL(KIND=4) :: DT
            END SUBROUTINE DETERMINE_ERF
          END INTERFACE 
        END MODULE DETERMINE_ERF__genmod
