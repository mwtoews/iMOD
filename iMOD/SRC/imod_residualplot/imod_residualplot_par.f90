MODULE MOD_RESIDUALPLOT_PAR

TYPE IPFRESOBJ
 REAL,POINTER,DIMENSION(:) :: X => NULL()              !## x-coordinate
 REAL,POINTER,DIMENSION(:) :: Y => NULL()              !## y-coordinate
 REAL,POINTER,DIMENSION(:) :: M => NULL()              !## computed values
 REAL,POINTER,DIMENSION(:) :: O => NULL()              !## observation
 REAL,POINTER,DIMENSION(:) :: W => NULL()              !## weight value
 INTEGER,POINTER,DIMENSION(:) :: L => NULL()           !## layer
 INTEGER,POINTER,DIMENSION(:) :: D => NULL()           !## date
 INTEGER :: NPOINTS                                    !## number of points in ipf
END TYPE IPFRESOBJ
TYPE(IPFRESOBJ),DIMENSION(:),ALLOCATABLE :: IPFR       !## stores original values from txtfile per ipf
REAL,DIMENSION(:),POINTER :: X,Y,Z                     !## values to be plotted
REAL,DIMENSION(:),POINTER :: X_TMP,Y_TMP,Z_TMP         !## values to be plotted
CHARACTER(LEN=256) :: INPUTFILE,BMPNAME                !## name of txtfile
INTEGER :: IPLOT,IWEIGHT                               !## plot type
INTEGER :: ITRANSIENT                                  !## transient/steadystate (lacking date)
INTEGER,POINTER,DIMENSION(:) :: ILAYER,IIPFS,IRDATE    !## all layer numbers/IPF-files to be read
INTEGER :: NLAYER,NIPFS,NRDATE                         !## amount of layers/IPF-files to be read
REAL :: GOF                                            !## goodness-of-fit with linear regression
REAL :: WC1,WC2                                        !## weight lower and upper class
REAL,DIMENSION(:),POINTER :: HCLASSES                  !## (user) defined histogram classes
REAL,DIMENSION(:),POINTER :: XCLASSES                  !## amount of points per defined histogram class for x-array

END MODULE MOD_RESIDUALPLOT_PAR