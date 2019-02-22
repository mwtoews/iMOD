MODULE MOD_MSPINSPECTOR_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MOD_IDF_PAR
USE RESOURCE

TYPE DATADXCTYPE
 INTEGER :: ILAY,IROW,ICOL,ID
END TYPE DATADXCTYPE
TYPE DXCTYPE
 TYPE(DATADXCTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE DXCTYPE
TYPE(DXCTYPE) :: DXC

TYPE DATAMODSVATTYPE
 INTEGER :: UNID,NUND,LYBE
END TYPE DATAMODSVATTYPE
TYPE MODSVATTYPE
 TYPE(DATAMODSVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE MODSVATTYPE
TYPE(MODSVATTYPE) :: MODSVAT

TYPE DATAIDFSVATTYPE
 INTEGER :: NUND,IROW,ICOL
END TYPE DATAIDFSVATTYPE
TYPE IDFSVATTYPE
 TYPE(DATAIDFSVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE IDFSVATTYPE
TYPE(IDFSVATTYPE) :: IDFSVAT

TYPE DATAAREASVATTYPE
 INTEGER :: LUSE,SOIL,METE,NUND
 REAL(KIND=DP_KIND) :: ARND,RZ,SURF
END TYPE DATAAREASVATTYPE
TYPE AREASVATTYPE
 TYPE(DATAAREASVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE AREASVATTYPE
TYPE(AREASVATTYPE) :: AREASVAT

CHARACTER(LEN=256) :: ROOT,LINE
CHARACTER(LEN=52) :: MNAME,MFNAME
TYPE(IDFOBJ) :: MSPIDF

END MODULE MOD_MSPINSPECTOR_PAR