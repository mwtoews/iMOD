MODULE MOD_AGGREGATE_PAR

 USE IMODVAR, ONLY : DP_KIND,SP_KIND
 USE MOD_IDF_PAR

 INTEGER,PARAMETER :: MF=2           !## max. number of subsystems
 INTEGER :: BLOCKSIZE=500000
 CHARACTER(LEN=256) :: FNAME,OUTPUTDIR
 TYPE FOBJ
  CHARACTER(LEN=256),DIMENSION(5) :: FILE        !## files top,bottom,kh,kv,va
  INTEGER,DIMENSION(5) :: ITYPE                  !## 0=idf, 1=constant value
  INTEGER :: ILAYER                              !## layer for insertion
  INTEGER :: METH                                !## method for insertion
  INTEGER,DIMENSION(5) :: DUPL_F,DUPL_T          !## duplicateformation,duplicatettype
  REAL(KIND=DP_KIND),DIMENSION(5) :: XVAL
 END TYPE FOBJ
 TYPE(FOBJ),DIMENSION(:),POINTER :: FLIST,ALIST,TLIST

TYPE ATOBJ
  INTEGER(KIND=2) :: IROW,ICOL
  REAL(KIND=SP_KIND) :: TP,BT,KH,KV,VA
 END TYPE ATOBJ
 TYPE FMMOBJ
  TYPE(ATOBJ),POINTER,DIMENSION(:) :: AT=>NULL()
  TYPE(ATOBJ),POINTER,DIMENSION(:) :: AT_DUMMY=>NULL()
 END TYPE FMMOBJ
 TYPE FMOBJ
  INTEGER :: IORDER
  TYPE(FMMOBJ),POINTER,DIMENSION(:) :: SF=>NULL()
 END TYPE FMOBJ
 TYPE(FMOBJ),ALLOCATABLE,DIMENSION(:) :: FM
 
 INTEGER :: IPRJ,IMODE,IWINDOW
 REAL(KIND=DP_KIND), PARAMETER :: EPS=1.0D0
 REAL(KIND=DP_KIND), PARAMETER :: KERROR=1.0D0,CERROR=1.0D0
 REAL(KIND=DP_KIND) :: CMIN,XMIN,YMIN,XMAX,YMAX,CELLSIZE
 
 CHARACTER(LEN=256) :: SORTFILE,IWHBFILE
 CHARACTER(LEN=256) :: INDIR,IWHBDIR,OUTDIR

 END MODULE MOD_AGGREGATE_PAR