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

TYPE DATAINFISVATTYPE
 INTEGER :: NUND
 REAL(KIND=DP_KIND) :: QINBASIC
END TYPE DATAINFISVATTYPE
TYPE INFISVATTYPE
 TYPE(DATAINFISVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE INFISVATTYPE
TYPE(INFISVATTYPE) :: INFISVAT

TYPE DATASCAPSVATTYPE
 INTEGER :: NUND,SVATAB,LYAB,SWNRAB
 REAL(KIND=DP_KIND) :: FMMXABGW,FMMXABSW,FXABGW,FXABSW
END TYPE DATASCAPSVATTYPE
TYPE SCAPSVATTYPE
 TYPE(DATASCAPSVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE SCAPSVATTYPE
TYPE(SCAPSVATTYPE) :: SCAPSVAT

TYPE DATALUSESVATTYPE
 INTEGER :: LU,VGLU
 REAL(KIND=DP_KIND) :: ALPHACRIT,P1FD,P2FD,P3HFD,P3LFD,P4FD,T3HFD,T3LFD,PBGSPLU,FREVSPLU,& 
                       GISPLU,TIGISPLU,RPSPLU,TDBGSPLU,TDEDSPLU,FECMNLU,ALBEDOLU,RSCDRYLU,&
                       RSCWETLU,KDIF,KDIR,ECMAXLU,ECSLOPLU
 CHARACTER(LEN=19) :: LUNA
END TYPE DATALUSESVATTYPE
TYPE LUSESVATTYPE
 TYPE(DATALUSESVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE LUSESVATTYPE
TYPE(LUSESVATTYPE) :: LUSESVAT


CHARACTER(LEN=256) :: ROOT,LINE
CHARACTER(LEN=52) :: MNAME,MFNAME
TYPE(IDFOBJ) :: MSPIDF

END MODULE MOD_MSPINSPECTOR_PAR