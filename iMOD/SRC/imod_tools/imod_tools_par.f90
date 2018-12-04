MODULE MOD_TOOLS_PAR

INTEGER :: TOOLSNTOPIC,TOOLSNDIR,TOOLSID
CHARACTER(LEN=256) :: TOOLSBROWSENAME,TOOLSDIR,ADIR,FNAME,LINE
CHARACTER(LEN=3),ALLOCATABLE,DIMENSION(:) :: CLAY
CHARACTER(LEN=4),ALLOCATABLE,DIMENSION(:) :: CYEAR
INTEGER,ALLOCATABLE,DIMENSION(:) :: ILAY,IYEAR
INTEGER,ALLOCATABLE,DIMENSION(:) :: COPYSHPIACT
INTEGER,POINTER,DIMENSION(:,:) :: IPERIOD
CHARACTER(LEN=3) :: DUMCLAY  !## cause lahey90 will not take this
CHARACTER(LEN=4) :: DUMCYEAR !## cause lahey90 will not take this
INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:) :: IPLIST

END MODULE MOD_TOOLS_PAR