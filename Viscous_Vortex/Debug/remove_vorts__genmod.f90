        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:04 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE REMOVE_VORTS__genmod
          INTERFACE 
            SUBROUTINE REMOVE_VORTS(PANELS,NV,XC2,YC2,XPN,YPN,GAMMA,    &
     &VORTX,VORTY,SGAM,UX0,NU,LIM,FLAG)
              INTEGER(KIND=4) :: NV
              INTEGER(KIND=4) :: PANELS
              REAL(KIND=4) :: XC2(PANELS+1)
              REAL(KIND=4) :: YC2(PANELS+1)
              REAL(KIND=4) :: XPN(NV)
              REAL(KIND=4) :: YPN(NV)
              REAL(KIND=4) :: GAMMA(NV)
              REAL(KIND=4) :: VORTX(NV)
              REAL(KIND=4) :: VORTY(NV)
              REAL(KIND=4) :: SGAM
              REAL(KIND=4) :: UX0(NV,2)
              REAL(KIND=4) :: NU
              REAL(KIND=4) :: LIM
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE REMOVE_VORTS
          END INTERFACE 
        END MODULE REMOVE_VORTS__genmod
