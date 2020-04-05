        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:03 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PSE4__genmod
          INTERFACE 
            SUBROUTINE PSE4(XC,YC,PANELS,NV,GAMMA,VORTX,VORTY,SIGMA,NU, &
     &DT,BOX,INDEX_VORTS,NB,NBV)
              INTEGER(KIND=4) :: NBV
              INTEGER(KIND=4) :: NB
              INTEGER(KIND=4) :: NV
              INTEGER(KIND=4) :: PANELS
              REAL(KIND=4) :: XC(PANELS+1)
              REAL(KIND=4) :: YC(PANELS+1)
              REAL(KIND=4) :: GAMMA(NV)
              REAL(KIND=4) :: VORTX(NV)
              REAL(KIND=4) :: VORTY(NV)
              REAL(KIND=4) :: SIGMA(NV)
              REAL(KIND=4) :: NU
              REAL(KIND=4) :: DT
              REAL(KIND=4) :: BOX(NB,12)
              INTEGER(KIND=4) :: INDEX_VORTS(NBV)
            END SUBROUTINE PSE4
          END INTERFACE 
        END MODULE PSE4__genmod
