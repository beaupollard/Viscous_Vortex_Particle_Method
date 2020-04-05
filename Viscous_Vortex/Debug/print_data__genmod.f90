        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 05 10:45:03 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PRINT_DATA__genmod
          INTERFACE 
            SUBROUTINE PRINT_DATA(INPUT,R,C,TITLE,LENG,UN,FLAG)
              INTEGER(KIND=4) :: LENG
              INTEGER(KIND=4) :: C
              INTEGER(KIND=4) :: R
              REAL(KIND=4) :: INPUT(R,C)
              CHARACTER(LEN=LENG) :: TITLE
              INTEGER(KIND=4) :: UN
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE PRINT_DATA
          END INTERFACE 
        END MODULE PRINT_DATA__genmod
