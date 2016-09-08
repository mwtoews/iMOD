MODULE MOD_RESIDUALPLOT_PAR

TYPE IPFOBJ
 REAL,POINTER,DIMENSION(:) :: X                       !## x-coordinate
 REAL,POINTER,DIMENSION(:) :: Y                       !## y-coordinate
 REAL,POINTER,DIMENSION(:) :: Z                       !## computed values
 REAL,POINTER,DIMENSION(:) :: O                       !## observation
 REAL,POINTER,DIMENSION(:) :: W                       !## weight value
 INTEGER,POINTER,DIMENSION(:) :: L                    !## layer
 CHARACTER(LEN=52),POINTER,DIMENSION(:) :: D          !## date
END TYPE IPFOBJ
TYPE(IPFOBJ),DIMENSION(:),ALLOCATABLE :: IPF          !## stores original values from txtfile per ipf
REAL,DIMENSION(:),ALLOCATABLE :: X,Y,Z                !## values to be plotted
CHARACTER(LEN=256) :: FNAME                           !## name of txtfile
INTEGER :: IPLOT                                      !## plot type

END MODULE MOD_RESIDUALPLOT_PAR