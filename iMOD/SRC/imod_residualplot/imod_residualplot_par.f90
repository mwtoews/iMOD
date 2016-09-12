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
REAL,DIMENSION(:),POINTER :: X,Y,Z                     !## values to be plotted
REAL,DIMENSION(:),POINTER :: X_TMP,Y_TMP,Z_TMP         !## values to be plotted
CHARACTER(LEN=256) :: INPUTFILE,BMPNAME                !## name of txtfile
INTEGER :: IPLOT                                       !## plot type
INTEGER :: ITRANSIENT                                  !## transient/steadystate (lacking date)
INTEGER,POINTER,DIMENSION(:) :: ILAYER                 !## all layer numbers to be read
INTEGER :: NLAYER                                      !## amount of layers to be read
REAL,DIMENSION(23) :: HCLASSES                         !## defined histogram classes
REAL,DIMENSION(22) :: XCLASSES                         !## amount of points per defined histogram class for x-array

END MODULE MOD_RESIDUALPLOT_PAR