MODULE MOD_RESIDUALPLOT_PAR

TYPE IPFRESOBJ
 REAL,POINTER,DIMENSION(:) :: X => NULL()              !## x-coordinate
 REAL,POINTER,DIMENSION(:) :: Y => NULL()              !## y-coordinate
 REAL,POINTER,DIMENSION(:) :: M => NULL()              !## computed values
 REAL,POINTER,DIMENSION(:) :: O => NULL()              !## observation
 REAL,POINTER,DIMENSION(:) :: W => NULL()              !## weight value
 INTEGER,POINTER,DIMENSION(:) :: L => NULL()           !## layer
 CHARACTER(LEN=52),POINTER,DIMENSION(:) :: D => NULL() !## date
 INTEGER :: NPOINTS                                    !## number of points in ipf
END TYPE IPFRESOBJ
TYPE(IPFRESOBJ),DIMENSION(:),ALLOCATABLE :: IPFR       !## stores original values from txtfile per ipf
REAL,DIMENSION(:),ALLOCATABLE :: X,Y,Z                 !## values to be plotted
CHARACTER(LEN=256) :: INPUTFILE,BMPNAME                !## name of txtfile
INTEGER :: IPLOT                                       !## plot type
INTEGER :: ITRANSIENT                                  !## transient/steadystate (lacking date)
INTEGER,DIMENSION(3) :: ICOL

END MODULE MOD_RESIDUALPLOT_PAR