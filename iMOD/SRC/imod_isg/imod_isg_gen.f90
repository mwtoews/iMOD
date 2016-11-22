!!  Copyright (C) Stichting Deltares, 2005-2016.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
MODULE MOD_ISGGEN

USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_CREATEDIR,UTL_GENLABELSREAD,UTL_GENLABELSGET,NV,NL,VAR,UTL_CAP,UTL_DIST, &
              UTL_DIRINFO_POINTER
USE MOD_OSD, ONLY : OSD_OPEN,ICF
USE MOD_ISG_UTL, ONLY : UTL_GETUNITSISG
USE MOD_ISG_PAR, ONLY : ISFR,TATTRIB1,TATTRIB2
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFGETVAL,IDFIROWICOL
USE MOD_IPF, ONLY : IPFALLOCATE,IPFREAD2
USE MOD_IPF_PAR

INTEGER,PARAMETER :: NIU=19
INTEGER,PARAMETER :: ISG=1,ISP=2,ISD1=3,ISD2=4,ISC1=5,ISC2=6,IOUT=11,IIDFZ=12, &
                     IST1=7,IST2=8,ISQ1=9,ISQ2=10,ICCF=13,IIDFW=14,IIDFB=15,   &
                     IIDFC=16,IIDFI=17,IIDFZ_BU=18,IIDFW_BU=19

CHARACTER(LEN=256),DIMENSION(NIU) :: FNAME
INTEGER,DIMENSION(NIU) :: IOS,IU
INTEGER,DIMENSION(7) :: DATCOL
INTEGER :: NP,NBRCH,DIMXY,SAMPLE,NPROF,ISTART,ISTOP,IBOT,ICDY,IINF,ISUMMER_BACKUP,IWINTER_BACKUP
CHARACTER(LEN=4) :: CWINTER,CSUMMER
REAL :: CDAY,INFFCT,RBOT,XSEARCH
REAL,ALLOCATABLE,DIMENSION(:) :: PNTX,PNTY
DOUBLE PRECISION,DIMENSION(2,2) :: XC,YC
INTEGER,ALLOCATABLE,DIMENSION(:) :: IP,IDNSEG
CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: CBID
CHARACTER(LEN=256) :: LINE
TYPE(IDFOBJ),DIMENSION(7) :: IDF
TYPE TYPEPROF
 REAL :: DISTANCE,BOTTOM,WLVLUP,WLVLDN
END TYPE TYPEPROF
TYPE(TYPEPROF),DIMENSION(:,:),ALLOCATABLE :: PROF
LOGICAL :: LDAT

CONTAINS

 !##=====================================================================
 SUBROUTINE ISGGEN_IPFTOISG(IPFFILE,ISGFILE,DATCOL,SDATE)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFFILE,ISGFILE
 INTEGER,INTENT(IN),DIMENSION(:) :: DATCOL
 CHARACTER(LEN=MAXLEN) :: CS1,CS2
 CHARACTER(LEN=52) :: WC
 CHARACTER(LEN=256) :: DIR
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: LISTNAME
 INTEGER(KIND=8),INTENT(IN) :: SDATE
 REAL :: WL1,BL1,WD1,WL2,BL2,WD2,X1,Y1,X2,Y2,HC1,HC2
 INTEGER :: I,J,IIPF 
 
 !## get list of ipf files
 IF(INDEX(IPFFILE,'*').GT.0)THEN
  WC=IPFFILE(INDEX(IPFFILE,'\',.TRUE.)+1:)
  DIR=IPFFILE(:INDEX(IPFFILE,'\',.TRUE.)-1)
  IF(.NOT.UTL_DIRINFO_POINTER(DIR,WC,LISTNAME,'F'))RETURN
  IF(SIZE(LISTNAME).LE.0)RETURN
  NIPF=SIZE(LISTNAME); CALL IPFALLOCATE()
  !## read all ipf files
  DO I=1,NIPF
   IPF(I)%FNAME=TRIM(DIR)//'\'//TRIM(LISTNAME(I))
   IPF(I)%XCOL =DATCOL(1)  !## x1
   IPF(I)%YCOL =DATCOL(2)  !## y1
   IPF(I)%ZCOL =DATCOL(5)  !## x2
   IPF(I)%Z2COL=DATCOL(6)  !## y2
   IPF(I)%QCOL =DATCOL(7)  !## stage
   !## read entire ipf
   IF(.NOT.IPFREAD2(I,1,1))THEN
    WRITE(*,'(A)') 'Cannot read '//TRIM(IPF(I)%FNAME); RETURN
   ENDIF
  ENDDO
 ELSE
  !## read ipf
  NIPF=1; CALL IPFALLOCATE()
  IPF(1)%FNAME=IPFFILE
  IPF(1)%XCOL =DATCOL(1)  !## x1
  IPF(1)%YCOL =DATCOL(2)  !## y1
  IPF(1)%ZCOL =DATCOL(5)  !## x2
  IPF(1)%Z2COL=DATCOL(6)  !## y2
  IPF(1)%QCOL =DATCOL(7)  !## stage
  !## read entire ipf
  IF(.NOT.IPFREAD2(1,1,1))THEN
   WRITE(*,'(A)') 'Cannot read '//TRIM(IPFFILE); RETURN
  ENDIF
 ENDIF
 
 !## fill in cross-sections per segment
 LDAT=.FALSE.;  NPROF=4 !8

 !## allocate pntx/pnty/ip/ibid
 DO I=1,2
  NBRCH=0; NP=0; IF(I.EQ.2)IP(0)=1
  DO IIPF=1,SIZE(IPF)
   DO J=1,IPF(IIPF)%NROW-1
    
    !## if not a label, use id-number
    IF(TRIM(IPF(IIPF)%INFO(DATCOL(3),J)).EQ.'')IPF(IIPF)%INFO(DATCOL(3),J)=IPF(IIPF)%INFO(DATCOL(4),J)

    READ(IPF(IIPF)%INFO(DATCOL(1),J),*)   X1
    READ(IPF(IIPF)%INFO(DATCOL(2),J),*)   Y1
    READ(IPF(IIPF)%INFO(DATCOL(1),J+1),*) X2
    READ(IPF(IIPF)%INFO(DATCOL(2),J+1),*) Y2

    CS1=UTL_CAP(IPF(IIPF)%INFO(DATCOL(4),J  ),'U')
    CS2=UTL_CAP(IPF(IIPF)%INFO(DATCOL(4),J+1),'U')

    !## duplicate coordinates, start new branch
    IF(TRIM(CS1).EQ.TRIM(CS2))THEN
     !## skip distance is zero
     IF(UTL_DIST(X1,Y1,X2,Y2).GT.0.0)THEN 
      IF(I.EQ.2)THEN
       PNTX(NP+1)=X1; PNTY(NP+1)=Y1
       PNTX(NP+2)=X2; PNTY(NP+2)=Y2
     
       IP(NBRCH+1)=NP+3
       CBID(NBRCH+1)='S_'//TRIM(IPF(IIPF)%INFO(DATCOL(3),I))//'_R_'//TRIM(ITOS(NBRCH+1))
       IDNSEG(NBRCH+1)=0 !NBRCH+2
     
       !## left side
       READ(IPF(IIPF)%INFO(DATCOL(5),J),*)     WD1
       READ(IPF(IIPF)%INFO(DATCOL(6),J),*)     BL1
       READ(IPF(IIPF)%INFO(DATCOL(7),J),*)     WL1
       READ(IPF(IIPF)%INFO(DATCOL(8),J),*)     HC1

       READ(IPF(IIPF)%INFO(DATCOL(5),J+1),*)   WD2
       READ(IPF(IIPF)%INFO(DATCOL(6),J+1),*)   BL2
       READ(IPF(IIPF)%INFO(DATCOL(7),J+1),*)   WL2
       READ(IPF(IIPF)%INFO(DATCOL(8),J+1),*)   HC2
       
       IF(BL1.GE.WL1)THEN
        WRITE(*,*) WL1,BL1,WL1-BL1
       ENDIF
       IF(BL2.GE.WL2)THEN
        WRITE(*,*) WL2,BL2,WL2-BL2
       ENDIF
       
       PROF(1,NBRCH+1)%DISTANCE=-WD1
       PROF(1,NBRCH+1)%BOTTOM  = WL1
       PROF(2,NBRCH+1)%DISTANCE=-HC1 !WD1
       PROF(2,NBRCH+1)%BOTTOM  = BL1
       PROF(3,NBRCH+1)%DISTANCE= HC2 !WD2
       PROF(3,NBRCH+1)%BOTTOM  = BL2
       PROF(4,NBRCH+1)%DISTANCE= WD2
       PROF(4,NBRCH+1)%BOTTOM  = WL2
      ENDIF
      NP=NP+2; NBRCH=NBRCH+1
     ENDIF
    ENDIF 
   ENDDO
  ENDDO
  IF(I.EQ.1)THEN
   ALLOCATE(PNTX(NP),PNTY(NP),IP(0:NBRCH),CBID(NBRCH),IDNSEG(NBRCH)); ALLOCATE(PROF(4,NBRCH))
  ENDIF
 ENDDO

 !## create isgfile from ipffile
 ISFR=1; CALL ISGGEN_CREATEISG(ISGFILE,2)

 END SUBROUTINE ISGGEN_IPFTOISG

 !##=====================================================================
 SUBROUTINE ISGGEN_GENTOISG(GENFNAME,OUTFILE)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: GENFNAME,OUTFILE
 INTEGER :: I,J
 LOGICAL :: LEX
 REAL :: X,Y
 INTEGER,DIMENSION(7) :: ILIST
 DATA ILIST/IIDFZ,IIDFW,IIDFB,IIDFC,IIDFI,IIDFZ_BU,IIDFW_BU/

 FNAME(1)=GENFNAME
 IU(1)=UTL_GETUNIT(); CALL OSD_OPEN(IU(1),FILE=FNAME(1),STATUS='OLD',ACTION='READ,DENYWRITE')

!## allocate memory
ALLOCATE(PNTX(0:1),PNTY(0:1))
CALL ISGGEN_READGEN(0)
REWIND(IU(1))
DEALLOCATE(PNTX,PNTY)
!## read entire file and initiate groups
ALLOCATE(PNTX(NP),PNTY(NP),IP(0:NBRCH),CBID(NBRCH)) 
CALL ISGGEN_READGEN(1)

WRITE(*,*) 'Read number of segments=',NBRCH

IF(.NOT.LDAT)THEN
 IU(ICCF)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ICCF),FILE=FNAME(ICCF),STATUS='OLD',ACTION='READ,DENYWRITE')
 READ(IU(ICCF),*)
 NPROF=0
 DO
  READ(IU(ICCF),*,IOSTAT=IOS(ICCF)) X,Y
  IF(IOS(ICCF).NE.0)EXIT
  NPROF=NPROF+1
 END DO
 ALLOCATE(PROF(NPROF,1))
 REWIND(IU(ICCF))
 READ(IU(ICCF),*)
 WRITE(*,'(/1X,2A10)') 'DISTANCE','BOTTOM'
 DO I=1,NPROF
  READ(IU(ICCF),*,IOSTAT=IOS(ICCF)) PROF(I,1)%DISTANCE,PROF(I,1)%BOTTOM
  WRITE(*,'(1X,2F10.2)') PROF(I,1)%DISTANCE,PROF(I,1)%BOTTOM
 END DO
 CLOSE(IU(ICCF))

 IF(IBOT.EQ.1)ILIST(3)=0
 IF(ICDY.EQ.1)ILIST(4)=0
 IF(IINF.EQ.1)ILIST(5)=0
 IF(IWINTER_BACKUP.EQ.0)ILIST(6)=0 
 IF(ISUMMER_BACKUP.EQ.0)ILIST(7)=0
 
 DO I=1,SIZE(ILIST) 
  IF(ILIST(I).NE.0)THEN
   J    =ILIST(I)
   IU(J)=UTL_GETUNIT()
   INQUIRE(FILE=FNAME(J),EXIST=LEX)
   IF(LEX)THEN
    IF(.NOT.IDFREAD(IDF(I),FNAME(J),0))RETURN
    IF(IDF(I)%IEQ.NE.0)STOP 'cannot use idf with ieq ne 0'
   ELSE
    WRITE(*,'(A)') 'Cannot find '//TRIM(FNAME(J)); STOP
   ENDIF
  ENDIF
 ENDDO

ELSE

 CALL UTL_GENLABELSREAD(GENFNAME(:INDEX(GENFNAME,'.',.TRUE.)-1)//'.DAT')
 IF(NV.LT.MAXVAL(DATCOL))STOP 'No enough columns in dat file'
 IF(.NOT.ASSOCIATED(VAR).OR.NL.LE.0)STOP 'No records found in dat file'

ENDIF

!## create isgfile from genfile
ISFR=0; CALL ISGGEN_CREATEISG(OUTFILE,1)

DEALLOCATE(PNTX,PNTY,IP,CBID)

END SUBROUTINE ISGGEN_GENTOISG

!##=====================================================================
SUBROUTINE ISGGEN_CREATEISG(OUTFILE,ISOURCE)
!##=====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: ISOURCE
CHARACTER(LEN=*),INTENT(IN) :: OUTFILE
INTEGER :: ISEG,ICRS,IB,NNP,ICLC,NSEG,NCLC,NCRS,IREFSD,IREFSC,I,IREC
REAL :: DIST

FNAME(ISG) =OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISG'
FNAME(ISP) =OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISP'
FNAME(ISD1)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISD1'
FNAME(ISD2)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISD2'
FNAME(ISC1)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISC1'
FNAME(ISC2)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISC2'
FNAME(IST1)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.IST1'
FNAME(IST2)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.IST2'
FNAME(ISQ1)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISQ1'
FNAME(ISQ2)=OUTFILE(:INDEX(OUTFILE,'.',.TRUE.)-1)//'.ISQ2'

FNAME(IOUT)='OUTPUT.TXT'

!## create folder
I=INDEX(OUTFILE,'\',.TRUE.)-1; CALL UTL_CREATEDIR(OUTFILE(:I))

CALL UTL_GETUNITSISG(IU,OUTFILE,'REPLACE')

IU(IOUT)=UTL_GETUNIT()
CALL OSD_OPEN(IU(IOUT),FILE=FNAME(IOUT),STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS(IOUT))

WRITE(IU(ISG),*) NBRCH,ISFR

WRITE(*,'(/A/)')

ISEG  =0  !## segment-points
ICRS  =0  !## cross-sections
ICLC  =0  !## data
IREFSD=0
IREFSC=0
NNP   =0
DO IB=1,NBRCH

 CALL ISGGEN_CREATEISP(IB,ISEG,NNP,NSEG,DIST)
 IF(ISFR.EQ.0)THEN
  CALL ISGGEN_CREATEISD1(ICLC,NCLC,IREFSD,DIST,IB)
  CALL ISGGEN_CREATEISC1(ICRS,NCRS,IREFSC,DIST,IB,1)
 ELSEIF(ISFR.EQ.1)THEN
  CALL ISGGEN_CREATEISD1_SFR(ICLC,NCLC,IREFSD,DIST,IB)
  CALL ISGGEN_CREATEISC1(ICRS,NCRS,IREFSC,DIST,IB,IB)
 ENDIF 

 LINE='"'//TRIM(CBID(IB))//'",'//TRIM(ITOS(ISEG-NSEG+1))//','//TRIM(ITOS(NSEG))//','// &
                                 TRIM(ITOS(ICLC-NCLC+1))//','//TRIM(ITOS(NCLC))//','// &
                                 TRIM(ITOS(ICRS-NCRS+1))//','//TRIM(ITOS(NCRS))//','// &
                                 TRIM(ITOS(0))          //','//TRIM(ITOS(0))   //','// &
                                 TRIM(ITOS(0))          //','//TRIM(ITOS(0))

 WRITE(IU(ISG),'(A)') TRIM(LINE)

 IF(NBRCH.GT.1000)WRITE(6,'(A,F10.4,A)') '+Progress ',REAL(IB*100)/REAL(NBRCH),'%         '   
END DO

WRITE(*,'(A)') 'Number of records in:'
WRITE(*,'(A, I10)') TRIM(FNAME(ISG)) //' ',NBRCH
WRITE(*,'(A, I10)') TRIM(FNAME(ISP)) //' ',NNP
WRITE(*,'(A,2I10)') TRIM(FNAME(ISD1))//' ',ICLC,IREFSD
WRITE(*,'(A,2I10)') TRIM(FNAME(ISC1))//' ',ICRS,IREFSC
WRITE(*,'(A,2I10)') TRIM(FNAME(IST1))//' ',0,0
WRITE(*,'(A,2I10)') TRIM(FNAME(ISQ1))//' ',0,0

WRITE(IU(ISP) ,REC=1)  8*256+247
WRITE(IU(ISD1),REC=1) 44*256+247
IF(ISFR.EQ.0)IREC= SIZE(TATTRIB1)*4
IF(ISFR.EQ.1)IREC=(SIZE(TATTRIB2)-1)*4+8
IREC=IREC*256+247
WRITE(IU(ISD2),REC=1) IREC 
WRITE(IU(ISC1),REC=1) 44*256+247 
WRITE(IU(ISC2),REC=1) 12*256+247
WRITE(IU(IST1),REC=1) 44*256+247 
WRITE(IU(IST2),REC=1) 12*256+247
WRITE(IU(ISQ1),REC=1) 44*256+247 
WRITE(IU(ISQ2),REC=1) 16*256+247
 
END SUBROUTINE ISGGEN_CREATEISG

!##=====================================================================
SUBROUTINE ISGGEN_CREATEISP(IB,ISEG,NNP,NSEG,DIST)
!##=====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: IB
INTEGER,INTENT(INOUT) :: ISEG,NNP
INTEGER,INTENT(OUT) :: NSEG
REAL,INTENT(OUT) :: DIST
INTEGER :: I,J

J   =0
DIST=0.0
DO I=IP(IB-1),IP(IB)-1
 IF(I.GT.IP(IB-1))THEN
  IF(PNTX(I).NE.PNTX(I-1).OR.PNTY(I).NE.PNTY(I-1))THEN
   DIST=DIST+SQRT((PNTX(I)-PNTX(I-1))**2.0+(PNTY(I)-PNTY(I-1))**2.0)
   ISEG=ISEG+1
   J   =J+1
   WRITE(IU(ISP),REC=ISEG+ICF) REAL(PNTX(I)),REAL(PNTY(I))
  ENDIF
 ELSE
  ISEG=ISEG+1
  J   =J+1
  WRITE(IU(ISP),REC=ISEG+ICF) REAL(PNTX(I)),REAL(PNTY(I))
 ENDIF
END DO

if(J.LE.1)THEN
 WRITE(*,*) 
ENDIF

NSEG=J
NNP =NNP+J

END SUBROUTINE ISGGEN_CREATEISP

!##=====================================================================
SUBROUTINE ISGGEN_CREATEISD1(ICLC,NCLC,IREFSD,DIST,IB)
!##=====================================================================
IMPLICIT NONE
INTEGER,INTENT(INOUT) :: ICLC,IREFSD
INTEGER,INTENT(OUT) :: NCLC
REAL,INTENT(IN) :: DIST
INTEGER,INTENT(IN) :: IB
REAL :: WL,WB,RS,FC,XDIST,XSAMPLE,FCT,D,DSAMPLE
INTEGER :: ID,I,ISAMPLE,IC,N,IY,ISTEP,IL,IOS
CHARACTER(LEN=8) :: CDATE
REAL :: XC,YC
CHARACTER(LEN=32) :: CNAME
CHARACTER(LEN=52) :: CID

NCLC=0; FC=INFFCT; RS=CDAY

!## add data for start and end segment point
IF(LDAT)THEN

 WRITE(CID,*) CBID(IB)
 CID=ADJUSTL(CID)

 CALL UTL_GENLABELSGET(CID,IL)
 IF(IL.EQ.0)RETURN

 READ(VAR(DATCOL(1),IL),*,IOSTAT=IOS) WL
 READ(VAR(DATCOL(3),IL),*,IOSTAT=IOS) WB

 N=1; ICLC=ICLC+1; NCLC=NCLC+1; IREFSD=IREFSD+1              

 !## from
 WRITE(CNAME,'(A8,I4.4)') 'ClcFROM:',IB
 WRITE(IU(ISD1),REC=ICLC+ICF) N,IREFSD,0.0,CNAME
 WRITE(IU(ISD2),REC=IREFSD+ICF) 19000101,WL,WB,RS,FC 

 READ(VAR(DATCOL(2),IL),*,IOSTAT=IOS) WL
 READ(VAR(DATCOL(4),IL),*,IOSTAT=IOS) WB

 N=1; ICLC=ICLC+1; NCLC=NCLC+1; IREFSD=IREFSD+1              

 !## to
 WRITE(CNAME,'(A8,I4.4)') 'ClcTO:',IB
 WRITE(IU(ISD1),REC=ICLC+ICF) N,IREFSD,DIST,CNAME
 WRITE(IU(ISD2),REC=IREFSD+ICF) 19000101,WL,WB,RS,FC 

 RETURN
ENDIF

ICLC=ICLC+1; NCLC=NCLC+1

N=((ISTOP-ISTART)+1)*2

CNAME=''; WRITE(CNAME,'(A8,I8.8)') 'ClcFROM:',IB
WRITE(IU(ISD1),REC=ICLC+ICF) N,IREFSD+1,0.0,CNAME

DO IY=ISTART,ISTOP
 DO IC=2,3
  IF(MOD(IC,2).EQ.0)THEN
   WRITE(CDATE,'(I4.4,A4)') IY,CSUMMER
   WL=ISGGEN_GETIDFVAL(PNTX(IP(IB-1)),PNTY(IP(IB-1)),1,IU(IIDFZ),ISTEP) 
   !## use backup if no point is found
   IF(WL.EQ.IDF(1)%NODATA.AND.ISUMMER_BACKUP.EQ.1)THEN; WL=ISGGEN_GETIDFVAL(PNTX(IP(IB-1)+1),PNTY(IP(IB-1)+1),6,IU(IIDFZ_BU),ISTEP); ENDIF
  ELSE
   WRITE(CDATE,'(I4.4,A4)') IY,CWINTER
   WL=ISGGEN_GETIDFVAL(PNTX(IP(IB-1)),PNTY(IP(IB-1)),2,IU(IIDFW),ISTEP)
   !## use backup if no point is found
   IF(WL.EQ.IDF(2)%NODATA.AND.IWINTER_BACKUP.EQ.1)THEN; WL=ISGGEN_GETIDFVAL(PNTX(IP(IB-1)+1),PNTY(IP(IB-1)+1),7,IU(IIDFW_BU),ISTEP); ENDIF
  ENDIF

  IF(IBOT.EQ.1)THEN; WB=WL-RBOT
  ELSE; WB=ISGGEN_GETIDFVAL(PNTX(IP(IB-1)),PNTY(IP(IB-1)),3,IU(IIDFB),ISTEP); ENDIF
  IF(ICDY.EQ.1)THEN; RS=CDAY
  ELSE; RS=ISGGEN_GETIDFVAL(PNTX(IP(IB-1)),PNTY(IP(IB-1)),4,IU(IIDFC),ISTEP); ENDIF
  IF(IINF.EQ.1)THEN; FC=INFFCT
  ELSE; FC=ISGGEN_GETIDFVAL(PNTX(IP(IB-1)),PNTY(IP(IB-1)),5,IU(IIDFI),ISTEP); ENDIF

  READ(CDATE,*) ID
  IREFSD=IREFSD+1
  WRITE(IU(ISD2),REC=IREFSD+ICF) ID,WL,WB,RS,FC
 ENDDO
END DO

DSAMPLE=DIST/SAMPLE
ISAMPLE=INT(DSAMPLE)

IF(ISAMPLE.GE.1)THEN

 !## stepsize
 DSAMPLE=DIST/REAL(ISAMPLE+1)

 XDIST  =0.0
 XSAMPLE=DSAMPLE

 DO I=IP(IB-1)+1,IP(IB)-1
  IF(PNTX(I).NE.PNTX(I-1).OR.PNTY(I).NE.PNTY(I-1))THEN

   D=SQRT((PNTX(I)-PNTX(I-1))**2.0+(PNTY(I)-PNTY(I-1))**2.0)

   IF(XDIST+D.GT.XSAMPLE)THEN

    DO

     FCT    = (XSAMPLE-XDIST)/D

     XC     = PNTX(I-1)+(PNTX(I)-PNTX(I-1))*FCT
     YC     = PNTY(I-1)+(PNTY(I)-PNTY(I-1))*FCT

     ICLC   = ICLC+1
     NCLC   = NCLC+1
     WRITE(CNAME,'(A4,2I8.8)') 'Clc:',IB,NCLC
     WRITE(IU(ISD1),REC=ICLC+ICF) N,IREFSD+1,XSAMPLE,CNAME

     DO IY=ISTART,ISTOP
      DO IC=2,3
       IF(MOD(IC,2).EQ.0)THEN
        WRITE(CDATE,'(I4.4,A4)') IY,CSUMMER
        WL=ISGGEN_GETIDFVAL(XC,YC,1,IU(IIDFZ),ISTEP)
       ELSE
        WRITE(CDATE,'(I4.4,A4)') IY,CWINTER
        WL=ISGGEN_GETIDFVAL(XC,YC,2,IU(IIDFW),ISTEP)
       ENDIF

       IF(IBOT.EQ.1)THEN; WB=WL-RBOT
       ELSE; WB=ISGGEN_GETIDFVAL(XC,YC,3,IU(IIDFB),ISTEP); ENDIF
       IF(ICDY.EQ.1)THEN; RS=CDAY
       ELSE; RS=ISGGEN_GETIDFVAL(XC,YC,4,IU(IIDFC),ISTEP); ENDIF
       IF(IINF.EQ.1)THEN; FC=INFFCT
       ELSE; FC=ISGGEN_GETIDFVAL(XC,YC,5,IU(IIDFI),ISTEP); ENDIF

       READ(CDATE,*) ID
       IREFSD=IREFSD+1
       WRITE(IU(ISD2),REC=IREFSD+ICF) ID,WL,WB,RS,FC
      END DO
     END DO

     XSAMPLE=XSAMPLE+DSAMPLE
     IF(XSAMPLE.GT.XDIST+D)EXIT

    ENDDO
   ENDIF

   XDIST=XDIST+D

  ENDIF
 ENDDO
ENDIF

!## to
ICLC  =ICLC+1
NCLC  =NCLC+1
WRITE(CNAME,'(A8,I8.8)') 'ClcTO:  ',IB
WRITE(IU(ISD1),REC=ICLC+ICF) N,IREFSD+1,DIST,CNAME

DO IY=ISTART,ISTOP
 DO IC=2,3
  IF(MOD(IC,2).EQ.0)THEN
   WRITE(CDATE,'(I4.4,A4)') IY,CSUMMER
   WL=ISGGEN_GETIDFVAL(PNTX(IP(IB)-1),PNTY(IP(IB)-1),1,IU(IIDFZ),ISTEP)
   !## use backup if no point is found
   IF(WL.EQ.IDF(1)%NODATA.AND.ISUMMER_BACKUP.EQ.1)THEN; WL=ISGGEN_GETIDFVAL(PNTX(IP(IB)-1),PNTY(IP(IB)-1),6,IU(IIDFZ_BU),ISTEP); ENDIF
  ELSE
   WRITE(CDATE,'(I4.4,A4)') IY,CWINTER
   WL=ISGGEN_GETIDFVAL(PNTX(IP(IB)-1),PNTY(IP(IB)-1),2,IU(IIDFW),ISTEP)
   !## use backup if no point is found
   IF(WL.EQ.IDF(2)%NODATA.AND.IWINTER_BACKUP.EQ.1)THEN; WL=ISGGEN_GETIDFVAL(PNTX(IP(IB)-1),PNTY(IP(IB)-1),7,IU(IIDFW_BU),ISTEP); ENDIF
  ENDIF

  IF(IBOT.EQ.1)THEN; WB=WL-RBOT
  ELSE; WB=ISGGEN_GETIDFVAL(PNTX(IP(IB)-1),PNTY(IP(IB)-1),3,IU(IIDFB),ISTEP); ENDIF
  IF(ICDY.EQ.1)THEN; RS=CDAY
  ELSE; RS=ISGGEN_GETIDFVAL(PNTX(IP(IB)-1),PNTY(IP(IB)-1),4,IU(IIDFC),ISTEP); ENDIF
  IF(IINF.EQ.1)THEN; FC=INFFCT
  ELSE; FC=ISGGEN_GETIDFVAL(PNTX(IP(IB)-1),PNTY(IP(IB)-1),5,IU(IIDFI),ISTEP); ENDIF

  READ(CDATE,*) ID
  IREFSD=IREFSD+1
  WRITE(IU(ISD2),REC=IREFSD+ICF) ID,WL,WB,RS,FC
 ENDDO
END DO

END SUBROUTINE ISGGEN_CREATEISD1

 !##=====================================================================
 SUBROUTINE ISGGEN_CREATEISD1_SFR(ICLC,NCLC,IREFSD,DIST,IB)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: ICLC,IREFSD
 INTEGER,INTENT(OUT) :: NCLC
 REAL,INTENT(IN) :: DIST
 INTEGER,INTENT(IN) :: IB
 REAL :: WLVL,BTML,WD,THCK,HCND,QFLW,QROF,PPTSW,ETSW
 INTEGER :: IDATE,UPSG,DWNS,ICALC,IPRI,N
 CHARACTER(LEN=32) :: CNAME
 CHARACTER(LEN=8) :: CTIME

 IDATE=19000000
 CTIME='00:00:00'
 QFLW=0.0
 QROF=0.0
 UPSG=0
 DWNS=IDNSEG(IB)
 !## rectangular profile (becomes 1 in sfr package but second option in dropdown menu)
 ICALC=2
 !## no diversion
 IPRI=1
 PPTSW=0.0
 ETSW=0.0
 
 NCLC=0

 N=1; ICLC=ICLC+1; NCLC=NCLC+1; IREFSD=IREFSD+1              

 !## water level
 WLVL=PROF(1,IB)%BOTTOM
 !## bottom level
 BTML=PROF(2,IB)%BOTTOM
 !## width (rectangular)
 WD=ABS(PROF(1,IB)%DISTANCE)

 THCK=0.10
 HCND=ABS(PROF(2,IB)%DISTANCE)
  
 !## from
 WRITE(CNAME,'(A8,I4.4)') 'ClcFROM:',IB
 WRITE(IU(ISD1),REC=ICLC+ICF) N,IREFSD,0.0,CNAME

 WRITE(IU(ISD2),REC=IREFSD+ICF) IDATE,CTIME,WLVL,BTML,WD,THCK,HCND,UPSG,DWNS,ICALC,IPRI,QFLW,QROF,PPTSW,ETSW

 N=1; ICLC=ICLC+1; NCLC=NCLC+1; IREFSD=IREFSD+1              

 !## water level
 WLVL=PROF(4,IB)%BOTTOM
 !## bottom level
 BTML=PROF(3,IB)%BOTTOM
 !## width (rectangular)
 WD=PROF(4,IB)%DISTANCE

 THCK=0.10
 HCND=ABS(PROF(3,IB)%DISTANCE)

 !## calculation option not to be specified here, irrelevant
 ICALC=1
 !## no diversion
 IPRI=1
 
 !## to
 WRITE(CNAME,'(A8,I4.4)') 'ClcTO:',IB
 WRITE(IU(ISD1),REC=ICLC+ICF) N,IREFSD,DIST,CNAME
 WRITE(IU(ISD2),REC=IREFSD+ICF) IDATE,CTIME,WLVL,BTML,WD,THCK,HCND,UPSG,DWNS,ICALC,IPRI,QFLW,QROF,PPTSW,ETSW

 END SUBROUTINE ISGGEN_CREATEISD1_SFR

!##=====================================================================
SUBROUTINE ISGGEN_CREATEISC1(ICRS,NCRS,IREFSC,DIST,IB,IPROF)
!##=====================================================================
IMPLICIT NONE
INTEGER,INTENT(INOUT) :: ICRS,IREFSC
INTEGER,INTENT(IN) :: IB,IPROF
INTEGER,INTENT(OUT) :: NCRS
REAL,INTENT(IN) :: DIST
INTEGER :: I,IOS,IL
REAL :: WDEPTH,TL,TR,BB,X
CHARACTER(LEN=32) :: CNAME
CHARACTER(LEN=52) :: CID

ICRS=ICRS+1; CNAME=''; WRITE(CNAME,'(A6,I6)') 'Prof. ',ICRS

!## add cross-section info from dat file
IF(LDAT)THEN

 WDEPTH=10.0

 CID=ADJUSTL(CBID(IB))

 CALL UTL_GENLABELSGET(CID,IL); IF(IL.EQ.0)RETURN

 READ(VAR(DATCOL(5),IL),*,IOSTAT=IOS) TL
 READ(VAR(DATCOL(6),IL),*,IOSTAT=IOS) TR
 READ(VAR(DATCOL(7),IL),*,IOSTAT=IOS) BB
 IF(TL.LE.0.0.OR.TR.LE.0.0.OR.BB.LE.0.0)THEN; WRITE(*,*) ' ERROR'; PAUSE; ENDIF
 TL=MAX(0.0,TL); TR=MAX(0.0,TR); BB=MAX(0.0,BB)
 
 WRITE(IU(ISC1),REC=ICRS+ICF) 4,IREFSC+1,DIST/2.0,CNAME 
 IREFSC=IREFSC+1
 BB=BB/2.0; X=TL*WDEPTH
 WRITE(IU(ISC2),REC=IREFSC+ICF) -X-BB,WDEPTH,25.0
 IREFSC=IREFSC+1
 WRITE(IU(ISC2),REC=IREFSC+ICF) -BB,0.0,25.0
 IREFSC=IREFSC+1
 WRITE(IU(ISC2),REC=IREFSC+ICF)  BB,0.0,25.0
 IREFSC=IREFSC+1
 X=TR*WDEPTH
 WRITE(IU(ISC2),REC=IREFSC+ICF)  X+BB,WDEPTH,25.0

ELSE

 WRITE(IU(ISC1),REC=ICRS+ICF) NPROF,IREFSC+1,DIST/2.0,CNAME 
 
 IF(ISFR.EQ.1)THEN
  PROF(1,IPROF)%DISTANCE=PROF(1,IPROF)%DISTANCE/2.0
  PROF(2,IPROF)%DISTANCE=PROF(1,IPROF)%DISTANCE

  WDEPTH=MAX(PROF(1,IPROF)%BOTTOM-PROF(2,IPROF)%BOTTOM, &
             PROF(4,IPROF)%BOTTOM-PROF(3,IPROF)%BOTTOM)

  PROF(1,IPROF)%BOTTOM=WDEPTH
  PROF(2,IPROF)%BOTTOM=0.0
!  WDEPTH=PROF(4,IPROF)%BOTTOM-PROF(3,IPROF)%BOTTOM
  PROF(3,IPROF)%BOTTOM=0.0
  PROF(4,IPROF)%BOTTOM=WDEPTH
  PROF(4,IPROF)%DISTANCE=PROF(4,IPROF)%DISTANCE/2.0
  PROF(3,IPROF)%DISTANCE=PROF(4,IPROF)%DISTANCE
 ENDIF
 
 DO I=1,NPROF
  IREFSC=IREFSC+1
  WRITE(IU(ISC2),REC=IREFSC+ICF) PROF(I,IPROF)%DISTANCE,PROF(I,IPROF)%BOTTOM,0.002
 END DO

ENDIF

NCRS=1

END SUBROUTINE ISGGEN_CREATEISC1

!##=====================================================================
SUBROUTINE ISGGEN_READGEN(ISTEP)
!##=====================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: ISTEP
INTEGER :: IOS,I,ID,NNP
CHARACTER(LEN=52) :: CID

I =0; NP=0; NBRCH=0
IF(ISTEP.EQ.1)IP(NBRCH)=1

DO
 READ(IU(1),*,IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
 IF(TRIM(UTL_CAP(CID,'U')).EQ.'END')EXIT
 !## increase groups, store branche id
 NBRCH=NBRCH+1
 READ(CID,*,IOSTAT=IOS) ID; IF(IOS.NE.0)ID=NBRCH
 IF(ISTEP.EQ.1)CBID(NBRCH)=TRIM(ITOS(ID))
 NNP=0
 DO
  READ(IU(1),*,IOSTAT=IOS) XC(1,1),YC(1,1)
  IF(IOS.NE.0)EXIT
  NNP=NNP+1
  NP =NP+1
  I  =I+ISTEP
  PNTX(I)=XC(1,1); PNTY(I)=YC(1,1)
 ENDDO
 IF(NNP.LE.1)THEN
  WRITE(*,*) 'ERROR: number of points in segment is le 1'
  WRITE(*,*) NNP,' '//TRIM(CID)
  STOP
 ENDIF
 IF(ISTEP.EQ.1)IP(NBRCH)=I+1
ENDDO

WRITE(*,*) 'Number of Segments Points: ',NP
WRITE(*,*) 'Number of Branches:        ',NBRCH

IF(ISTEP.EQ.1)CLOSE(IU(1))

END SUBROUTINE ISGGEN_READGEN

!#####=================================================================
REAL FUNCTION ISGGEN_GETIDFVAL(XC,YC,I,IU,ISTEP)
!#####=================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: I,IU
INTEGER,INTENT(INOUT) :: ISTEP
REAL,INTENT(IN) :: XC,YC
INTEGER :: ICOL,IROW,IR,IC

CALL IDFIROWICOL(IDF(I),IROW,ICOL,XC,YC)

IF(ICOL.GT.0.AND.ICOL.LE.IDF(I)%NCOL.AND. &
   IROW.GT.0.AND.IROW.LE.IDF(I)%NROW)THEN

 ISGGEN_GETIDFVAL=IDFGETVAL(IDF(I),IROW,ICOL)

 !## search if no value has been found
 IF(ISGGEN_GETIDFVAL.EQ.IDF(I)%NODATA)THEN
  ISTEP=0
ISTEPLOOP: DO
   ISTEP=ISTEP+1
   IF(REAL(ISTEP)*IDF(I)%DX.GT.XSEARCH)EXIT
   DO IR=MAX(IROW-ISTEP,1),MIN(IROW+ISTEP,IDF(I)%NROW)
    DO IC=MAX(ICOL-ISTEP,1),MIN(ICOL+ISTEP,IDF(I)%NCOL)
     ISGGEN_GETIDFVAL=IDFGETVAL(IDF(I),IR,IC)
     IF(ISGGEN_GETIDFVAL.NE.IDF(I)%NODATA)EXIT ISTEPLOOP
    END DO
   ENDDO
  ENDDO ISTEPLOOP
 ENDIF

ELSE
 ISGGEN_GETIDFVAL=IDF(I)%NODATA
ENDIF

END FUNCTION ISGGEN_GETIDFVAL

END MODULE MOD_ISGGEN