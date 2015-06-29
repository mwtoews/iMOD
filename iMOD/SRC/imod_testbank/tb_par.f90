MODULE MOD_TB_PAR

CHARACTER(LEN=256) :: CONFNAME,OUTMAP,REPLACESTRING
INTEGER :: IUOUT
INTEGER :: NEXE
REAL :: THRESHOLD
TYPE TEXE
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=52) :: ALIAS
 INTEGER :: IACT
END TYPE TEXE
TYPE(TEXE),ALLOCATABLE,DIMENSION(:) :: EXE
INTEGER :: NRUN
TYPE TRUN
 CHARACTER(LEN=256) :: FNAME
END TYPE TRUN
TYPE(TRUN),POINTER,DIMENSION(:) :: RUN,RUN_BU

END MODULE MOD_TB_PAR