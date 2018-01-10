!####============================================================
MODULE MOD_GLBVAR
REAL,SAVE :: DELT,SIMCSIZE
INTEGER,SAVE :: ISS       !## 1=STEADYSTATE 2=TRANSIENT
CHARACTER(LEN=20),SAVE :: CDATE
CHARACTER(LEN=256),SAVE :: LINE
REAL,DIMENSION(4),SAVE :: SIMBOX
LOGICAL,SAVE :: LQD
END MODULE MOD_GLBVAR

!####============================================================
MODULE MOD_ISGVAR
!####============================================================
 INTEGER,PARAMETER :: MAXITEMS=4 !excl. date
 INTEGER :: NISGH,ISEG,NSEG,ID,NXY
 CHARACTER(LEN=50) :: CISGH
 CHARACTER(LEN=16),DIMENSION(MAXITEMS) :: ATTRIB
 REAL,ALLOCATABLE,DIMENSION(:) :: X,Y
 REAL :: X1,Y1,X2,Y2,ISGX,ISGY
 TYPE DWPTYPE
  REAL :: DIST
  INTEGER :: NCROS
 END TYPE DWPTYPE
 TYPE CROSTYPE
  REAL :: DIST,KM,BOTTOM,ZP
 END TYPE CROSTYPE
 TYPE(DWPTYPE),DIMENSION(:),ALLOCATABLE :: DWP
 TYPE(CROSTYPE),DIMENSION(:,:),ALLOCATABLE :: CROS
 REAL,DIMENSION(:,:),POINTER :: isgdata,isgdata_bu
END MODULE MOD_ISGVAR

!####============================================================
MODULE MOD_BASVAR
!####============================================================
 INTEGER,SAVE :: NCOL,NROW
 REAL,ALLOCATABLE,DIMENSION(:),SAVE :: DELR,DELC
END MODULE MOD_BASVAR
