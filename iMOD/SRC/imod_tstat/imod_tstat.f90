!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MOD_TSTAT

USE DATEVAR
USE MOD_UTL, ONLY : UTL_FILLDATES,UTL_GDATE,UTL_MESSAGEHANDLE,OSD_OPEN,UTL_GETUNIT,ITOS,RTOS,UTL_GETMED,UTL_GETHIST, &
 UTL_WAITMESSAGE,UTL_WSELECTFILE,UTL_INSIDEPOLYGON,UTL_GETUNIQUE_INT,UTL_GETMED_INVERSE,UTL_JDATETOIDATE,UTL_STDEF,  &
 UTL_CREATEDIR,UTL_IDATETOJDATE
USE MOD_TSTAT_PAR
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFGETXYVAL,IDFDEALLOCATE,IDFNULLIFY,IDFIROWICOL
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IPF_PAR, ONLY : IPF,NIPF,ASSF
USE MOD_IPF, ONLY : IPFALLOCATE,IPFREAD2,IPFDEALLOCATE
USE MOD_IPFASSFILE, ONLY : IPFOPENASSFILE,IPFREADASSFILELABEL,IPFREADASSFILE,IPFCLOSEASSFILE,IPFASSFILEALLOCATE,ASSF
USE MOD_POLYGON_UTL, ONLY : POLYGON1INIT,POLYGON1SAVELOADSHAPE,POLYGON1CLOSE
USE MOD_POLYGON_PAR, ONLY : MAXSHPCRD,SHPNO,SHPXC,SHPYC,SHPNCRD,SHPID
USE IMOD, ONLY : IDFINIT

CONTAINS

 !###======================================================================
 SUBROUTINE TSTATRESIDUAL()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: ROOT,FNAME
 INTEGER :: II,I,J,JJ,IU,JU,KU,IOS,ILAY,N,IL,IL1,IL2,NPOP,IZ,NZ,IROW,ICOL
 REAL :: M,H,W,VAR,MEAN,XC,YC,MH,MM,STVDH,MDH,XCOR,YCOR,ZCOR ,DYNH,DYNM 
 REAL,DIMENSION(11) :: PERC,XPERC
 REAL,DIMENSION(17) :: HIST,XHIST
 DATA PERC/0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0/
 DATA HIST/-1000.0,-5.0,-2.5,-1.0,-0.75,-0.5,-0.25,-0.10,0.0,0.10,0.25,0.5,0.75,1.0,2.5,5.0,1000.0/
 REAL,ALLOCATABLE,DIMENSION(:) :: X,XW
 REAL,DIMENSION(2) :: XMED
 TYPE(IDFOBJ) :: IDF
 
 IF(SOBSDATE.EQ.0.AND.EOBSDATE.EQ.0)THEN
  SOBSDATE=-10E6; EOBSDATE=10E6
 ELSE
  SOBSDATE=UTL_IDATETOJDATE(SOBSDATE)
  EOBSDATE=UTL_IDATETOJDATE(EOBSDATE)
 ENDIF
 
 CALL IDFNULLIFY(IDF)
 IF(TRIM(POINTERIDF).NE.'')THEN
  IF(.NOT.IDFREAD(IDF,POINTERIDF,1))RETURN
  NZ=NZONE
 ELSE
  NZ=1; NZONE=0
 ENDIF
 
 CALL UTL_CREATEDIR(OUTNAME(:INDEX(OUTNAME,'\',.TRUE.)))
 IF(ICOLLECT.EQ.1)CALL UTL_CREATEDIR(OUTNAME(:INDEX(OUTNAME,'\',.TRUE.))//'timeseries')
 
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=OUTNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  WRITE(*,'(A)') 'Error: Cannot write file: ['//TRIM(OUTNAME)//']'; RETURN
 ENDIF
 
 ALLOCATE(RES(100)) 
 
 DO IZ=1,NZ
 
  WRITE(JU,'(A52,9(A1,A15),A1,A10)') 'Name',',','MH',',','MM',',','MH-MM',',','STVDH',',','DYNH',',','DYNM',',','DYNH-DYNM',',','XCOR',',','W',',','NUMBER'

  N=0
  DO II=1,SIZE(IPFFILE)
   NIPF=1; CALL IPFALLOCATE() 
  
   ROOT=IPFFILE(II)(:INDEX(IPFFILE(II),'\',.TRUE.)-1)
   IPF(1)%FNAME=IPFFILE(II)
   IPF(1)%XCOL =IHCOL(II); IPF(1)%YCOL=IMCOL(II); IPF(1)%ZCOL=IWCOL(II)
   IPF(1)%Z2COL=ILCOL(II); IPF(1)%QCOL=IWCOL(II)

   IF(.NOT.IPFREAD2(1,1,0))RETURN
   
   IF(IPF(1)%ACOL.EQ.0)THEN
    IF(.NOT.TSTATRESIDUAL_POINTER(N,IPF(1)%NROW))RETURN
    DO I=1,IPF(1)%NROW
     
     !## overrule weigh if not specified
     IF(W_TYPE(II).EQ.0)IPF(1)%XYZ(3,I)=1.0
     
     READ(IPF(1)%INFO(1,I),*) XC; READ(IPF(1)%INFO(2,I),*) YC
     !## check whether insize appropriate zone
     IF(NZONE.GT.0)THEN
      CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
      IF(IDF%X(ICOL,IROW).NE.IZONE(IZ))CYCLE
     ENDIF   

     W=IPF(1)%XYZ(3,I); IF(W.GT.0.0.AND.W_TYPE(II).EQ.2)W=1.0/SQRT(W); W=MAX(0.0,W)
     ILAY=0; IF(ILCOL(II).GT.0)ILAY=IPF(1)%XYZ(4,I)
     H=IPF(1)%XYZ(1,I)
     M=IPF(1)%XYZ(2,I)
     IF(H.EQ.HNODATA)CYCLE
     N=N+1
     RES(N)%M   =   M
     RES(N)%H   =   H
     RES(N)%DH  =  (H-M)
     RES(N)%DHW =W*(H-M)
     RES(N)%W   =   W
     RES(N)%ILAY=   ILAY
     RES(N)%X   =   XC
     RES(N)%Y   =   YC

     WRITE(6,'(A,F10.2)') '+Progress ',REAL(I)*100.0/REAL(IPF(1)%NROW)  

    ENDDO
   
   ELSE
    CALL IPFASSFILEALLOCATE(1)
    DO I=1,IPF(1)%NROW

     !## overrule weigh if not specified
     IF(W_TYPE(II).EQ.0)IPF(1)%XYZ(3,I)=1.0

     READ(IPF(1)%INFO(1,I),*) XC; READ(IPF(1)%INFO(2,I),*) YC
     !## check whether inside appropriate zone
     IF(NZONE.GT.0)THEN
      CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
      IF(IDF%X(ICOL,IROW).NE.IZONE(IZ))CYCLE
     ENDIF   
     FNAME=TRIM(ROOT)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'.'//TRIM(IPF(1)%FEXT)
     W=IPF(1)%XYZ(3,I); IF(W.GT.0.0.AND.W_TYPE(II).EQ.2)W=1.0/SQRT(W); W=MAX(0.0,W)
     ILAY=0; IF(ILCOL(II).GT.0)ILAY=IPF(1)%XYZ(4,I)
     IF(IPFOPENASSFILE(IU,1,FNAME).AND. &
        IPFREADASSFILELABEL(IU,1,FNAME).AND.  &
        IPFREADASSFILE(IU,1,FNAME))THEN
      IF(.NOT.TSTATRESIDUAL_POINTER(N,ASSF(1)%NRASS))RETURN

      IF(ALLOCATED(X))DEALLOCATE(X); ALLOCATE(X(ASSF(1)%NRASS))

      !## get diff
      JJ=0; DO J=1,ASSF(1)%NRASS
       M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)   
       IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
        IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN; JJ=JJ+1; X(JJ)=H-M; ENDIF      
       ENDIF
      ENDDO
      CALL UTL_STDEF(X,JJ,ASSF(1)%NODATA(3),STVDH,MDH,NPOP)

      !## get dynamic measure
      JJ=0; DO J=1,ASSF(1)%NRASS
       M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)   
       IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
        IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN; JJ=JJ+1; X(JJ)=H; ENDIF      
       ENDIF
      ENDDO
      CALL UTL_GETMED(X,JJ,ASSF(1)%NODATA(3),(/10.0,90.0/),2,NPOP,XMED); DYNH=XMED(2)-XMED(1)

      !## get dynamic model
      JJ=0; DO J=1,ASSF(1)%NRASS
       M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)   
       IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
        IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN; JJ=JJ+1; X(JJ)=M; ENDIF      
       ENDIF
      ENDDO
      CALL UTL_GETMED(X,JJ,ASSF(1)%NODATA(3),(/10.0,90.0/),2,NPOP,XMED); DYNM=XMED(2)-XMED(1)

      !## get mean
      JJ=0; MH=0.0; MM=0.0
      DO J=1,ASSF(1)%NRASS
       M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)
       IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
        IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN; MH=MH+H; MM=MM+M; JJ=JJ+1; ENDIF      
       ENDIF
      ENDDO

      XCOR=0.0; YCOR=0.0; ZCOR=0.0

      IF(JJ.GT.0)THEN
       MH=MH/REAL(JJ); MM=MM/REAL(JJ)

       !## compute cross-correlation
       IF(JJ.GT.1)THEN
        DO J=1,ASSF(1)%NRASS
         M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)   
         IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
          IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN
           XCOR=XCOR+(MM-M)*(MH-H)
           YCOR=YCOR+(MM-M)**2.0
           ZCOR=ZCOR+(MH-H)**2.0
          ENDIF
         ENDIF
        ENDDO
        IF(YCOR.NE.0.0.AND.ZCOR.NE.0.0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
       ENDIF
       
       IF(W.GT.0.0)THEN
        N  =N+1
        RES(N)%M   =   STVDH 
        RES(N)%H   =   DYNH 
        RES(N)%DH  =   MDH
        RES(N)%DHW =   DYNH-DYNM
        RES(N)%DHW=0.0
        RES(N)%COR =   XCOR
        RES(N)%MM  =   MM
        RES(N)%MH  =   MH
        RES(N)%W   =   W
        RES(N)%ILAY=ILAY
        RES(N)%X   =XC
        RES(N)%Y   =YC
        RES(N)%CID ='timeseries\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I)) 
       ENDIF       
       
      ENDIF
      WRITE(JU,'(A52,9(A1,G15.7),A1,I10)') TRIM(IPF(1)%INFO(IPF(1)%ACOL,I)),',',MH,',',MM,',',MH-MM,',',STVDH,',',DYNH,',',DYNM,',',DYNH-DYNM,',',XCOR,',',W,',',JJ
      IF(ICOLLECT.EQ.1)THEN
       CALL IOSCOPYFILE(TRIM(ROOT)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'.'//TRIM(IPF(1)%FEXT), &
                        OUTNAME(:INDEX(OUTNAME,'\',.TRUE.))//'timeseries\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'.'//TRIM(IPF(1)%FEXT))
      ENDIF
     ENDIF

     WRITE(6,'(A,F10.2)') '+Progress ',REAL(I)*100.0/REAL(IPF(1)%NROW)  

    ENDDO
    CALL IPFDEALLOCATE()
    CALL IPFCLOSEASSFILE()
   ENDIF
  ENDDO
 
  IF(NZONE.GT.0)WRITE(JU,'(/A,I10)') 'Statistics for zone ',IZONE(IZ)
  WRITE(JU,'(A,I10)') 'Number of measurements ',N

  IL1=MINVAL(RES(1:N)%ILAY); IL2=MAXVAL(RES(1:N)%ILAY)
  IF(ALLOCATED(X))DEALLOCATE(X); IF(ALLOCATED(XW))DEALLOCATE(XW); ALLOCATE(X(N),XW(N))
  DO J=1,5
   IF(J.EQ.1)WRITE(JU,'(/A)') 'Mean Differences' !Weighed values'
   IF(J.EQ.2)WRITE(JU,'(/A)') 'Absolute Mean Differences' !Weighed Absolute values'
   IF(J.EQ.3)WRITE(JU,'(/A)') 'Dynamics Difference' !Normal values'
   IF(J.EQ.4)WRITE(JU,'(/A)') 'Absolute Dynamics Difference' !values'
   IF(J.EQ.5)WRITE(JU,'(/A)') 'Correlation Coefficients'
   WRITE(JU,'(2A10,2A15,99G15.7)') 'Layer','NPop.','Mean','St.Dev.',(PERC(I),I=1,SIZE(PERC))
   DO IL=IL1,IL2
    II=0; DO I=1,N
     IF(RES(I)%ILAY.EQ.IL)THEN
      II=II+1
      IF(J.EQ.1)X(II)=    RES(I)%DH
      IF(J.EQ.2)X(II)=ABS(RES(I)%DH)
      IF(J.EQ.3)X(II)=    RES(I)%DHW
      IF(J.EQ.4)X(II)=ABS(RES(I)%DHW)
      IF(J.EQ.5)X(II)=    RES(I)%COR
     ENDIF
    ENDDO
    IF(II.GT.0)THEN
     CALL UTL_STDEF(X,II,10.0E10,VAR,MEAN,NPOP)
     CALL UTL_GETMED(X,II,10.0E10,PERC,SIZE(PERC),JJ,XPERC)
    ELSE
     XPERC=0.0;MEAN=0.0; VAR=0.0; XHIST=0.0
    ENDIF
    WRITE(JU,'(2I10,99F15.7)') IL,NPOP,MEAN,VAR,(XPERC(I),I=1,SIZE(PERC))
   ENDDO
  ENDDO
  WRITE(JU,'(A)')
  DO J=1,5
   IF(J.EQ.1)WRITE(JU,'(/A)') 'Mean Differences' !Weighed values'
   IF(J.EQ.2)WRITE(JU,'(/A)') 'Absolute Mean Differences' !Weighed Absolute values'
   IF(J.EQ.3)WRITE(JU,'(/A)') 'Dynamics Difference' !Normal values'
   IF(J.EQ.4)WRITE(JU,'(/A)') 'Absolute Dynamics Difference' !values'
   IF(J.EQ.5)WRITE(JU,'(/A)') 'Correlation Coefficients'
   WRITE(JU,'(2A10,2(A15,A1),99(G15.7,A1))') 'Layer','NPop.','Mean','St.Dev.',(PERC(I),I=1,SIZE(PERC))
   IF(J.EQ.1)X(1:N)=RES(1:N)%DH
   IF(J.EQ.2)X(1:N)=ABS(RES(1:N)%DH)
   IF(J.EQ.3)X(1:N)=RES(1:N)%DHW
   IF(J.EQ.4)X(1:N)=ABS(RES(1:N)%DHW)
   IF(J.EQ.5)X(1:N)=    RES(1:N)%COR
   CALL UTL_STDEF(X,N,10.0E10,VAR,MEAN,NPOP)
   CALL UTL_GETMED(X,N,10.0E10,PERC,SIZE(PERC),JJ,XPERC)
   WRITE(JU,'(10X,I10,99(G15.7,A1))') NPOP,MEAN,VAR,(XPERC(I),I=1,SIZE(PERC))
  ENDDO
 
  !## write ipf files
  DO IL=IL1,IL2
   IF(NZONE.EQ.0)THEN
    FNAME='residual_ilay'//TRIM(ITOS(IL))//'.IPF'
   ELSE
    FNAME='residual_ilay'//TRIM(ITOS(IL))//'_izone'//TRIM(ITOS(IZONE(IZ)))//'.IPF'
   ENDIF
   KU=UTL_GETUNIT(); CALL OSD_OPEN(KU,FILE=OUTNAME(:INDEX(OUTNAME,'\',.TRUE.))//TRIM(FNAME),STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
   J=0; DO I=1,N; IF(RES(I)%ILAY.EQ.IL)J=J+1; ENDDO
   WRITE(KU,*) J
   WRITE(KU,*) 10+ICOLLECT
   WRITE(KU,*) 'X';  WRITE(KU,*) 'Y'
   IF(ICOLLECT.EQ.1)WRITE(KU,*) 'IDENTIFICATION'
   WRITE(KU,*) 'STVDH'; WRITE(KU,*) 'DYNH'; WRITE(KU,*) 'DYNH-DYNM'
   WRITE(KU,*) 'MH-MM'; WRITE(KU,*) 'MH'; WRITE(KU,*) 'MM'; WRITE(KU,*) 'COR'; WRITE(KU,*) 'W'
   WRITE(KU,*) ICOLLECT*3,',TXT'
   J=0; DO I=1,N
    IF(RES(I)%ILAY.EQ.IL)THEN
     J=J+1
     IF(ICOLLECT.EQ.0)WRITE(KU,'(10(G15.7,A1))') RES(I)%X,',',RES(I)%Y,',',RES(I)%M,',',RES(I)%H,',',RES(I)%DHW,',',RES(I)%DH,',',RES(I)%MH,',',RES(I)%MM,',',RES(I)%COR,',',RES(I)%W 
     IF(ICOLLECT.EQ.1)WRITE(KU,'(10(G15.7,A))')  RES(I)%X,',',RES(I)%Y,',"'//TRIM(RES(I)%CID)//'",',RES(I)%M,',',RES(I)%H,',',RES(I)%DHW,',',RES(I)%DH,',',RES(I)%MH,',',RES(I)%MM,',',RES(I)%COR,',',RES(I)%W 
    ENDIF
   ENDDO
   CLOSE(KU)
  ENDDO

  WRITE(*,'(/A,I10/)') 'Finished processing zone ',IZ
 ENDDO
 CLOSE(JU)

 DEALLOCATE(RES,X,XW)
 
 END SUBROUTINE TSTATRESIDUAL
 
 !###======================================================================
 LOGICAL FUNCTION TSTATRESIDUAL_POINTER(N,M)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,M
 INTEGER :: IOS
 
 TSTATRESIDUAL_POINTER=.TRUE.
 IF(SIZE(RES).GE.N+M)RETURN

 TSTATRESIDUAL_POINTER=.FALSE.
 ALLOCATE(RES_DUMMY(N+M),STAT=IOS)
 IF(IOS.NE.0)RETURN
 RES_DUMMY(1:SIZE(RES))=RES(1:SIZE(RES))
 DEALLOCATE(RES)
 RES=>RES_DUMMY
  
 TSTATRESIDUAL_POINTER=.TRUE.
  
 END FUNCTION TSTATRESIDUAL_POINTER 
 
 !###======================================================================
 SUBROUTINE TSTAT1MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE

 CALL WDIALOGSELECT(MESSAGE%WIN)

 SELECT CASE(ITYPE)

  CASE (FIELDCHANGED)
   IF(MESSAGE%VALUE1.EQ.MESSAGE%VALUE2)THEN
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_INTEGER2,IDF_MENU2,IDF_INTEGER1)
      CALL UTL_FILLDATES(IDF_INTEGER2,IDF_MENU2,IDF_INTEGER1)    !## correct startdates
     CASE (IDF_INTEGER4,IDF_MENU3,IDF_INTEGER3)
      CALL UTL_FILLDATES(IDF_INTEGER4,IDF_MENU3,IDF_INTEGER3)    !## correct enddates
     CASE (IDF_CHECK1,IDF_CHECK2,IDF_CHECK3,IDF_CHECK4)
      CALL TSTAT1FIELDS()
      !## put statistical variables ... depends on ipf(a) and/or ipf(b)
      IF(MESSAGE%VALUE1.EQ.IDF_CHECK1.OR. &
         MESSAGE%VALUE2.EQ.IDF_CHECK2)CALL TSTAT1PUTVARIABLES()
     CASE (IDF_STRING1,IDF_STRING2)
      CALL WDIALOGGETSTRING(MESSAGE%VALUE1,FNAME1)
      IF(MESSAGE%VALUE1.EQ.IDF_STRING1)CALL TSTAT1READATTRIBUTES(IDF_OPEN1,FNAME1)
      IF(MESSAGE%VALUE1.EQ.IDF_STRING2)CALL TSTAT1READATTRIBUTES(IDF_OPEN2,FNAME1)
      CALL TSTAT1FIELDS()
     CASE (IDF_STRING3,IDF_STRING4)
      CALL TSTAT1FIELDS()
     CASE (IDF_MENU1)
      CALL TSTAT1FIELDS()
    END SELECT
   ENDIF

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDHELP)
       CALL IMODGETHELP('5.16','TMO.CompTimeStat')
    CASE (IDF_OPEN1,IDF_OPEN2,IDF_OPEN3,IDF_OPEN4,IDF_OPEN5)
     CALL TSTAT1OPEN(MESSAGE%VALUE1)
    CASE (IDCANCEL)
     CALL TSTAT1CLOSE()
    CASE (IDOK)
     CALL TSTAT1APPLY(0)
    CASE (ID_AUTO)
     CALL TSTAT1GETDATES()
   END SELECT

 END SELECT

 END SUBROUTINE TSTAT1MAIN

 !###======================================================================
 SUBROUTINE TSTAT1OPEN(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: I

 SELECT CASE (ID)
  CASE (IDF_OPEN1,IDF_OPEN2)
   FNAME1=TRIM(PREFVAL(1))//'\*.ipf'
   IF(UTL_WSELECTFILE('iMOD Point Map (*.ipf)|*.ipf|',LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME1,&
                    'Load iMOD Point Map (*.ipf)'))THEN
    IF(ID.EQ.IDF_OPEN1)CALL WDIALOGPUTSTRING(IDF_STRING1,FNAME1)
    IF(ID.EQ.IDF_OPEN2)CALL WDIALOGPUTSTRING(IDF_STRING2,FNAME1)
    CALL TSTAT1READATTRIBUTES(ID,FNAME1)
   ENDIF
  CASE (IDF_OPEN3)
   FNAME1=TRIM(PREFVAL(1))//'\*.idf'
   IF(UTL_WSELECTFILE('iMOD Map (*.idf)|*.idf|',LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME1,&
                    'Load iMOD Map (*.idf)'))CALL WDIALOGPUTSTRING(IDF_STRING3,FNAME1)
  CASE (IDF_OPEN4)
   FNAME1=TRIM(PREFVAL(1))//'\*.gen'
   IF(UTL_WSELECTFILE('ESRI Generate Polygon File (*.gen)|*.gen|',LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME1,&
                    'Load ESRI Generate Polygon File (*.gen)'))CALL WDIALOGPUTSTRING(IDF_STRING4,FNAME1)
  CASE (IDF_OPEN5)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK4,I)
   IF(I.EQ.0)THEN
    FNAME1=TRIM(PREFVAL(1))//'\*.ipf'
    IF(UTL_WSELECTFILE('iMOD Point Map (*.ipf)|*.ipf|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME1,&
                     'Save as iMOD Point Map (*.ipf)'))CALL WDIALOGPUTSTRING(IDF_STRING5,FNAME1)
   ELSEIF(I.EQ.1)THEN
    FNAME1=TRIM(PREFVAL(1))//'\*.gen'
    IF(UTL_WSELECTFILE('ESRI Generate Polygon File (*.gen)|*.gen|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME1,&
                     'Save as ESRI Generate Polygon File (*.gen)'))CALL WDIALOGPUTSTRING(IDF_STRING5,FNAME1)
   ENDIF
 END SELECT

 END SUBROUTINE TSTAT1OPEN

 !###======================================================================
 SUBROUTINE TSTAT1READATTRIBUTES(ID,FNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 LOGICAL :: LEX

 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  IF(ID.EQ.IDF_OPEN1)THEN
   CALL WDIALOGCLEARFIELD(IDF_MENU4)
   CALL WDIALOGFIELDSTATE(IDF_MENU4,0)
   CALL WDIALOGPUTSTRING(IDF_LABEL6,'No Attributes')
  ELSEIF(ID.EQ.IDF_OPEN2)THEN
   CALL WDIALOGCLEARFIELD(IDF_MENU5)
   CALL WDIALOGFIELDSTATE(IDF_MENU5,0)
   CALL WDIALOGPUTSTRING(IDF_LABEL7,'No Attributes')
  ENDIF
  RETURN
 ENDIF

 NIPF=1
 CALL IPFALLOCATE()
 IPF(1)%FNAME=FNAME

 !## read only header
 LEX=IPFREAD2(1,0,1)
 IF(LEX)THEN
  IF(IPF(1)%ACOL.LE.0)LEX=.FALSE.
 ENDIF
 IF(ID.EQ.IDF_OPEN1)THEN
  CALL WDIALOGPUTMENU(IDF_MENU4,IPF(1)%ATTRIB,IPF(1)%NCOL,MAX(1,IPF(1)%ACOL))
  CALL WDIALOGFIELDSTATE(IDF_MENU4,1)
  CALL WDIALOGPUTSTRING(IDF_LABEL6,'Relate Attribute:')
 ENDIF
 IF(ID.EQ.IDF_OPEN2)THEN
  CALL WDIALOGPUTMENU(IDF_MENU5,IPF(1)%ATTRIB,IPF(1)%NCOL,MAX(1,IPF(1)%ACOL))
  CALL WDIALOGFIELDSTATE(IDF_MENU5,1)
  CALL WDIALOGPUTSTRING(IDF_LABEL7,'Relate Attribute:')
 ENDIF
 CALL IPFDEALLOCATE()

 END SUBROUTINE TSTAT1READATTRIBUTES

 !###======================================================================
 SUBROUTINE TSTAT1APPLY(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER,PARAMETER :: NOPTIONS=9
 INTEGER,PARAMETER :: NRESULTS=25
 INTEGER,DIMENSION(NOPTIONS) :: IOPTIONS
 CHARACTER(LEN=25),DIMENSION(NOPTIONS) :: COPTIONS
 CHARACTER(LEN=25),DIMENSION(NRESULTS) :: CRESULTS
 INTEGER,DIMENSION(NRESULTS) :: IRELATE
 REAL,ALLOCATABLE,DIMENSION(:,:) :: XRESULT,XSUM
 REAL,ALLOCATABLE,DIMENSION(:) :: XPERC
 INTEGER :: I,J,K,ID,IY,IM,IIPF,JIPF,IOS,IU,NADD,N,NAJ,JROW,IRAT,IRAT1,ITYPE,JPOL, &
            MXPOL,NPERC,CFN_N_ELEM,IPERC,NU,EXITCODE
 REAL :: NODATA,MV
 REAL,PARAMETER :: NODATA_DEFAULT=-999.99
 INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:) :: IPOL
 TYPE(WIN_MESSAGE) :: MESSAGE
 LOGICAL :: LEX
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 CHARACTER(LEN=52),POINTER,DIMENSION(:,:) :: VAR
 
 DATA COPTIONS/'Auto-Correlation IPF A','Auto-Correlation IPF B','Cross','P50 (IPF A)','P50 (IPF B)', &
               '(n)GxG (IPF A)','(n)GxG (IPF B)','TopBot (IPF A)','TopBot (IPF B)'/
 DATA CRESULTS/'AutoCor_IPFA','AutoCorMeanLag_IPFA','AutoCorNP_IPFA',&    !1-3
               'AutoCor_IPFB','AutoCorMeanLag_IPFB','AutoCorNP_IPFB',&    !4-6
               'CrossCor','CrossCorMeanLag','CrossCorNP',&                !7-9
               'P50_IPFA','P50_IPFB',&                                    !10-11
               'GHG_IPFA','GLG_IPFA','nGxG_IPFA',&                        !12-14
               'GHG_IPFB','GLG_IPFB','nGxG_IPFB',&                        !15-17
               'TopDate_IPFA','TopValue_IPFA','BotDate_IPFA','BotValue_IPFA',& !18-21
               'TopDate_IPFB','TopValue_IPFB','BotDate_IPFB','BotValue_IPFB'/ !22-25
 DATA IRELATE/1,1,1,2,2,2,3,3,3,4,5,6,6,6,7,7,7,8,8,8,8,9,9,9,9/  !## number of results yielding according to irelate!

 ALLOCATE(IDF(1)); CALL IDFNULLIFY(IDF(1)); IDF(1)%IU=0
 IF(TRIM(SURFACELEVEL).NE.'')THEN
  IF(.NOT.IDFREAD(IDF(1),SURFACELEVEL,0))THEN
   WRITE(*,'(A)') 'Cannot open: '//TRIM(SURFACELEVEL); RETURN
  ENDIF
 ENDIF

 IF(IBATCH.EQ.0)THEN
  CALL WDIALOGGETCHECKBOX(IDF_CHECK4,IGEN)
  ICOLDATE='1'
  ICOLVARS='2'
 ENDIF
 
 IF(IGEN.EQ.1)THEN
  IF(IBATCH.EQ.0)CALL WDIALOGGETSTRING(IDF_STRING6,CXPERC)
  NPERC=CFN_N_ELEM(' ,;',3,CXPERC)
  ALLOCATE(XPERC(NPERC))
  READ(CXPERC,*,IOSTAT=IOS) (XPERC(I),I=1,NPERC)
  IF(IOS.NE.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot translate percentiles to numeric values'//CHAR(13)// &
    TRIM(CXPERC),'Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Cannot translate percentiles to numeric values:'//TRIM(CXPERC)
   RETURN
  ENDIF
 ENDIF

 IF(IBATCH.EQ.0)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'Are you sure to continue?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF
 
 !## read polygon
 SHPNO=0
 IF(IGEN.EQ.1)THEN
  CALL WDIALOGGETSTRING(IDF_STRING4,GENNAME)
  IF(IBATCH.EQ.0)CALL POLYGON1INIT()
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,0,GENNAME,VAR)
  MXPOL=SIZE(SHPXC,1)
 ELSE
  MXPOL=1
 ENDIF

 IOPTIONS=0
 DO I=1,SIZE(COPTIONS)
  DO J=1,SIZE(CVARS)
   IF(IVARS(J).EQ.1)THEN
    IF(INDEX(TRIM(CVARS(J)),TRIM(COPTIONS(I))).GT.0)THEN
     IOPTIONS(I)=1
     EXIT
    ENDIF
   ENDIF
  ENDDO
 ENDDO

 !## determine number of result-columns
 NADD=0
 DO I=1,SIZE(IRELATE); IF(IOPTIONS(IRELATE(I)).EQ.1)NADD=NADD+1; ENDDO

 IF(IBATCH.EQ.0)CALL WDIALOGGETSTRING(IDF_STRING5,OUTNAME)
 IU=UTL_GETUNIT()
 IF(IGEN.EQ.0)CALL OSD_OPEN(IU,FILE=OUTNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IGEN.EQ.1)CALL OSD_OPEN(IU,FILE=OUTNAME(:INDEX(OUTNAME,'.',.TRUE.))//'dat',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open given result file (E)'//CHAR(13)// &
   '['//TRIM(OUTNAME)//']','Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Cannot open given result file (E) ['//TRIM(OUTNAME)//']'
  RETURN
 ENDIF

 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(0)

 IF(TSTAT1READIPFS(IIPF,JIPF,IBATCH))THEN

  IF(IBATCH.EQ.0)THEN
   CALL WDIALOGGETMENU(IDF_MENU4,RELATECOLIPF1)
   CALL WDIALOGGETMENU(IDF_MENU5,RELATECOLIPF2)
   CALL WDIALOGGETREAL(IDF_REAL1,XLAG)
   CALL WDIALOGGETREAL(IDF_REAL2,DLAG)
   CALL WDIALOGGETINTEGER(IDF_INTEGER1,ID)
   CALL WDIALOGGETINTEGER(IDF_INTEGER2,IY)
   CALL WDIALOGGETMENU(IDF_MENU2,IM)
   DMY1=IY*10000+IM*100+ID
   CALL WDIALOGGETINTEGER(IDF_INTEGER3,ID)
   CALL WDIALOGGETINTEGER(IDF_INTEGER4,IY)
   CALL WDIALOGGETMENU(IDF_MENU3,IM)
   DMY2=IY*10000+IM*100+ID
  ENDIF
  
  IF(ALLOCATED(XRESULT))DEALLOCATE(XRESULT)
  IF(ALLOCATED(XSUM))   DEALLOCATE(XSUM)
  ALLOCATE(XRESULT(NRESULTS,IPF(IIPF)%NROW))
  IF(SHPNO.GT.0)ALLOCATE(XSUM(IPF(IIPF)%NROW,NRESULTS))

  IF(ALLOCATED(IPOL))DEALLOCATE(IPOL)
  ALLOCATE(IPOL(IPF(IIPF)%NROW))

  !## start processing each of the records ... ipf A or ipf B (iipf-value)
  IPF(IIPF)%IP=INT(0,1)
  IPOL=INT(0,2)
  JROW=0
  IRAT=0
  IRAT1=0
  EXITCODE=0
  DO I=1,IPF(IIPF)%NROW
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   !## check whether point is in particular polygon (jpol)
   IF(TSTAT1UTL_INSIDEPOLYGON(IPF(IIPF)%XYZ(1,I),IPF(IIPF)%XYZ(2,I),JPOL))THEN
    MV=-9999.0; IF(IDF(1)%IU.NE.0)MV=IDFGETXYVAL(IDF(1),IPF(IIPF)%XYZ(1,I),IPF(IIPF)%XYZ(2,I))

    !## store polygonnumber for particular point
    IF(IGEN.EQ.1)IPOL(I)=INT(SHPID(JPOL),2) 
    !## point selected
    IPF(IIPF)%IP(I)=INT(1,1)

    LEX=.TRUE.

    !## both available
    IF(IIPF+JIPF.EQ.3)THEN
     !## search related line ...
     LEX=TSTAT1RELATE(IIPF,JIPF,I,JROW,RELATECOLIPF1,RELATECOLIPF2)
     IF(LEX)THEN
      FNAME1=TRIM(DIR1)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//   '.'//TRIM(IPF(IIPF)%FEXT)
      FNAME2=TRIM(DIR2)//'\'//TRIM(IPF(JIPF)%INFO(IPF(JIPF)%ACOL,JROW))//'.'//TRIM(IPF(JIPF)%FEXT)
     ENDIF
    ELSE
     IF(IIPF.EQ.1)FNAME1=TRIM(DIR1)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//'.'//TRIM(IPF(IIPF)%FEXT)
     IF(IIPF.EQ.2)FNAME2=TRIM(DIR2)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//'.'//TRIM(IPF(IIPF)%FEXT)
    ENDIF
    
    IF(LEX)THEN
     IF(IIPF.EQ.1)THEN; INQUIRE(FILE=FNAME1,EXIST=LEX); ENDIF
     IF(JIPF.EQ.2)THEN; IF(LEX)INQUIRE(FILE=FNAME2,EXIST=LEX); ENDIF
    ENDIF
    
    IF(LEX)THEN

     !##
     write(*,*) 'irow=',i
     write(*,'(a)') '1 '//trim(fname1)
     write(*,'(a)') '2 '//trim(fname2)
!     fname1='d:\dump\B28D0417001.txt'
!     fname2='d:\dump\B28D0417001.txt'
     !## textfiles ...
     CALL IMOD_TSTAT_CALC(FNAME1,&  !## in txt1
                          FNAME2,&  !## in txt1
                          ICOLDATE, &
                          ICOLVARS, &
                          IOPTIONS,&!## ioptions
                          NODATA_DEFAULT,& !## missing value
                          XRESULT(1,I),& !## results values per row in the ipf
                          XLAG,&    !## lag
                          DLAG,&    !## lagwidth
                          0,&       !## startdate top/bot
                          0,&       !## enddate top/bot
                          0.0,&     !##
                          0.0,&     !##
                          0.0,&     !##
                          DMY1,&    !## startdate
                          DMY2,&    !## enddate
                          3,&       !## number of measures for a year minimal for gxg
                          50.0,&    !## percentile A
                          50.0, &   !## percentile B
                          EXITCODE)
     IF(EXITCODE.NE.0)THEN
      IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error occured processing statistics!','Error')
      IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error occured processing statistics!'
      EXIT
     ENDIF
     if(mv.le.-9999)then
       XRESULT(12,I)=XRESULT(12,I); XRESULT(13,I)=XRESULT(13,I)
       XRESULT(15,I)=XRESULT(15,I); XRESULT(16,I)=XRESULT(16,I)
     else
       XRESULT(12,I)=MV-XRESULT(12,I); XRESULT(13,I)=MV-XRESULT(13,I)
       XRESULT(15,I)=MV-XRESULT(15,I); XRESULT(16,I)=MV-XRESULT(16,I)
     endif
    ELSE
     XRESULT(:,I)=NODATA_DEFAULT
    ENDIF
   ENDIF
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,IPF(IIPF)%NROW,'Progress ')
  ENDDO  !## do i=1,ipf(iipf)%nrow

  IF(IGEN.EQ.0)THEN

   !## write result in ipf
   LINE=TRIM(ITOS(SUM(INT(IPF(IIPF)%IP))))
   WRITE(IU,'(A)') TRIM(LINE)
   I=IPF(IIPF)%NCOL+NADD
   IF(IIPF+JIPF.EQ.3)THEN
    IF(IOPTIONS(IRELATE(10)).EQ.1.AND.IOPTIONS(IRELATE(11)).EQ.1)I=I+1
    IF(IOPTIONS(IRELATE(12)).EQ.1.AND.IOPTIONS(IRELATE(15)).EQ.1)I=I+1
    IF(IOPTIONS(IRELATE(13)).EQ.1.AND.IOPTIONS(IRELATE(16)).EQ.1)I=I+1
    IF(IOPTIONS(IRELATE(14)).EQ.1.AND.IOPTIONS(IRELATE(17)).EQ.1)I=I+1
   ENDIF
   LINE=TRIM(ITOS(I))
   WRITE(IU,'(A)') TRIM(LINE)
   !## write default columns from IPF(iipf)
   DO I=1,IPF(IIPF)%NCOL; WRITE(IU,'(A)') TRIM(IPF(IIPF)%ATTRIB(I)); ENDDO
   !## write extra columns for results
   DO I=1,SIZE(IRELATE); IF(IOPTIONS(IRELATE(I)).EQ.1)WRITE(IU,'(A)') TRIM(CRESULTS(I)); ENDDO
   !## write diff-columns if two ipf were selected
   IF(IIPF+JIPF.EQ.3)THEN
    IF(IOPTIONS(IRELATE(10)).EQ.1.AND.IOPTIONS(IRELATE(11)).EQ.1)WRITE(IU,'(A)') 'diff_P50'
    IF(IOPTIONS(IRELATE(12)).EQ.1.AND.IOPTIONS(IRELATE(15)).EQ.1)WRITE(IU,'(A)') 'diff_GHG'
    IF(IOPTIONS(IRELATE(13)).EQ.1.AND.IOPTIONS(IRELATE(16)).EQ.1)WRITE(IU,'(A)') 'diff_GLG'
    IF(IOPTIONS(IRELATE(14)).EQ.1.AND.IOPTIONS(IRELATE(17)).EQ.1)WRITE(IU,'(A)') 'diff_nGXG'
   ENDIF
   WRITE(IU,'(A)') '0,txt'
   !## write data
   DO I=1,IPF(IIPF)%NROW
    LINE='"'//TRIM(IPF(IIPF)%INFO(1,I))//'"'
    DO J=2,IPF(IIPF)%NCOL; LINE=TRIM(LINE)//',"'//TRIM(IPF(IIPF)%INFO(J,I))//'"'; ENDDO
    DO J=1,SIZE(IRELATE); IF(IOPTIONS(IRELATE(J)).EQ.1)LINE=TRIM(LINE)//',"'//TRIM(RTOS(XRESULT(J,I),'F',3))//'"'; ENDDO

    IF(IIPF+JIPF.EQ.3)THEN
     IF(IOPTIONS(IRELATE(10)).EQ.1.AND.IOPTIONS(IRELATE(11)).EQ.1)THEN
      IF(XRESULT(10,I).NE.NODATA_DEFAULT.AND.XRESULT(11,I).NE.NODATA_DEFAULT)THEN
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(XRESULT(10,I)-XRESULT(11,I),'F',3))//'"'
      ELSE
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(NODATA_DEFAULT,'F',3))//'"'
      ENDIF
     ENDIF
     IF(IOPTIONS(IRELATE(12)).EQ.1.AND.IOPTIONS(IRELATE(15)).EQ.1)THEN
      IF(XRESULT(12,I).NE.NODATA_DEFAULT.AND.XRESULT(15,I).NE.NODATA_DEFAULT)THEN
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(XRESULT(12,I)-XRESULT(15,I),'F',3))//'"'
      ELSE
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(NODATA_DEFAULT,'F',3))//'"'
      ENDIF
     ENDIF
     IF(IOPTIONS(IRELATE(13)).EQ.1.AND.IOPTIONS(IRELATE(16)).EQ.1)THEN
      IF(XRESULT(13,I).NE.NODATA_DEFAULT.AND.XRESULT(16,I).NE.NODATA_DEFAULT)THEN
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(XRESULT(13,I)-XRESULT(16,I),'F',3))//'"'
      ELSE
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(NODATA_DEFAULT,'F',3))//'"'
      ENDIF
     ENDIF
     IF(IOPTIONS(IRELATE(14)).EQ.1.AND.IOPTIONS(IRELATE(17)).EQ.1)THEN
      IF(XRESULT(14,I).NE.NODATA_DEFAULT.AND.XRESULT(17,I).NE.NODATA_DEFAULT)THEN
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(XRESULT(14,I)-XRESULT(17,I),'F',3))//'"'
      ELSE
       LINE=TRIM(LINE)//',"'//TRIM(RTOS(NODATA_DEFAULT,'F',3))//'"'
      ENDIF
     ENDIF
    ENDIF

    WRITE(IU,'(A)') TRIM(LINE)

   ENDDO

  ELSEIF(IGEN.EQ.1)THEN

   !## write polygon!
   CALL POLYGON1SAVELOADSHAPE(ID_SAVESHAPE,0,OUTNAME,VAR) 
   !## write default columns from IPF(iipf)
   LINE='ID'

   !## write header
   DO IPERC=1,NPERC

    !## write extra columns for results
    DO I=1,SIZE(IRELATE)
     IF(IOPTIONS(IRELATE(I)).EQ.1)LINE=TRIM(LINE)//','//TRIM(CRESULTS(I))//'('//TRIM(RTOS(XPERC(IPERC),'F',2))//')'
    ENDDO
    !## write diff-columns if two ipf were selected
    IF(IIPF+JIPF.EQ.3)THEN
     IF(IOPTIONS(IRELATE(10)).EQ.1.AND.IOPTIONS(IRELATE(11)).EQ.1) &
      LINE=TRIM(LINE)//',diff_P50('//TRIM(RTOS(XPERC(IPERC),'F',2))//')'
     IF(IOPTIONS(IRELATE(12)).EQ.1.AND.IOPTIONS(IRELATE(15)).EQ.1) &
      LINE=TRIM(LINE)//',diff_GHG('//TRIM(RTOS(XPERC(IPERC),'F',2))//')'
     IF(IOPTIONS(IRELATE(13)).EQ.1.AND.IOPTIONS(IRELATE(16)).EQ.1) &
      LINE=TRIM(LINE)//',diff_GLG('//TRIM(RTOS(XPERC(IPERC),'F',2))//')'
     IF(IOPTIONS(IRELATE(14)).EQ.1.AND.IOPTIONS(IRELATE(17)).EQ.1) &
      LINE=TRIM(LINE)//',diff_nGXG('//TRIM(RTOS(XPERC(IPERC),'F',2))//')'
     IF(IOPTIONS(IRELATE(12)).EQ.1.AND.IOPTIONS(IRELATE(15)).EQ.1) &
      LINE=TRIM(LINE)//',diff_Dynamic('//TRIM(RTOS(XPERC(IPERC),'F',2))//')'
    ENDIF
   ENDDO

   WRITE(IU,'(A)') TRIM(LINE)//',n'

   !## get unique polygons id's
   CALL UTL_GETUNIQUE_INT(SHPID,SHPNO,NU)
 
   IRAT =0
   IRAT1=0
   !## determine percentile values
   DO J=1,NU 
    CALL WMESSAGEPEEK(ITYPE,MESSAGE)

    !## id-number
    LINE=TRIM(ITOS(SHPID(J))) 
    
    DO IPERC=1,NPERC

     !## write data -- ONLY if inside current polygon
     XSUM=0.0
     N=0
     DO I=1,IPF(IIPF)%NROW
      IF(IPOL(I).EQ.SHPID(J))THEN
       N=N+1
       DO K=1,NRESULTS; XSUM(N,K)=XRESULT(K,I); ENDDO
      ENDIF
     ENDDO
     DO K=1,NRESULTS
      NODATA=MINVAL(XSUM(:,K))-1.0 !## make sure all data is used in median computation
      CALL UTL_GETMED(XSUM(1,K),N,NODATA,(/XPERC(IPERC)/),1,NAJ,XSUM(1,K))
     ENDDO
     DO K=1,SIZE(IRELATE); IF(IOPTIONS(IRELATE(K)).EQ.1)LINE=TRIM(LINE)//','//TRIM(RTOS(XSUM(1,K),'F',3)); ENDDO

     !## write diff-columns if two ipf were selected
     IF(IIPF+JIPF.EQ.3)THEN
      !## all nodata
      XSUM=NODATA_DEFAULT
      !## compute differences
      N=0
      DO I=1,IPF(IIPF)%NROW
       IF(IPOL(I).EQ.SHPID(J))THEN
        N=N+1
        IF(XRESULT(10,I).NE.NODATA_DEFAULT.AND.XRESULT(11,I).NE.NODATA_DEFAULT)THEN
         XSUM(N,1)=ABS(XRESULT(10,I)-XRESULT(11,I)) !## p50
        ENDIF
        IF(XRESULT(12,I).NE.NODATA_DEFAULT.AND.XRESULT(15,I).NE.NODATA_DEFAULT)THEN
         XSUM(N,2)=ABS(XRESULT(12,I)-XRESULT(15,I)) !## ghg
        ENDIF
        IF(XRESULT(13,I).NE.NODATA_DEFAULT.AND.XRESULT(16,I).NE.NODATA_DEFAULT)THEN
         XSUM(N,3)=ABS(XRESULT(13,I)-XRESULT(16,I)) !## glg
        ENDIF
        IF(XRESULT(14,I).NE.NODATA_DEFAULT.AND.XRESULT(17,I).NE.NODATA_DEFAULT)THEN
         XSUM(N,4)=ABS(XRESULT(14,I)-XRESULT(17,I)) !## ngxg
        ENDIF

        IF(XRESULT(12,I).NE.NODATA_DEFAULT.AND.XRESULT(15,I).NE.NODATA_DEFAULT.AND. & !## ghg
           XRESULT(13,I).NE.NODATA_DEFAULT.AND.XRESULT(16,I).NE.NODATA_DEFAULT)THEN
         XSUM(N,5)=ABS( (XRESULT(13,I)-XRESULT(12,I)) - & !## dynamiek A
                        (XRESULT(16,I)-XRESULT(15,I)) )   !## dynamiek B
        ENDIF

       ENDIF
      ENDDO
      !## compute percentiles of differences
      DO K=1,5 !4 
       IF(IINVERSE.EQ.0)CALL UTL_GETMED(XSUM(1,K),N,NODATA_DEFAULT,(/XPERC(IPERC)/),1,NAJ,XSUM(5,K))
       IF(IINVERSE.EQ.1)CALL UTL_GETMED_INVERSE(XSUM(1,K),N,NODATA_DEFAULT,(/XPERC(IPERC)/),1,NAJ,XSUM(5,K))
       XSUM(1,K)=XSUM(5,K)
      ENDDO

      !## P50-IPFA - P50-IPFB
      IF(IOPTIONS(IRELATE(10)).EQ.1.AND.IOPTIONS(IRELATE(11)).EQ.1)THEN
       IF(XSUM(1,1).NE.NODATA_DEFAULT)THEN 
        LINE=TRIM(LINE)//','//TRIM(RTOS(XSUM(1,1),'F',3)) 
       ELSE
        LINE=TRIM(LINE)//','//TRIM(RTOS(NODATA_DEFAULT,'F',3))
       ENDIF
      ENDIF
      !## GHG_IPFA - GHG_IPFB
      IF(IOPTIONS(IRELATE(12)).EQ.1.AND.IOPTIONS(IRELATE(15)).EQ.1)THEN
       IF(XSUM(1,2).NE.NODATA_DEFAULT)THEN 
        LINE=TRIM(LINE)//','//TRIM(RTOS(XSUM(1,2),'F',3)) 
       ELSE
        LINE=TRIM(LINE)//','//TRIM(RTOS(NODATA_DEFAULT,'F',3))
       ENDIF
      ENDIF
      !## GLG_IPFA - GLG_IPFB
      IF(IOPTIONS(IRELATE(13)).EQ.1.AND.IOPTIONS(IRELATE(16)).EQ.1)THEN
       IF(XSUM(1,3).NE.NODATA_DEFAULT)THEN 
        LINE=TRIM(LINE)//','//TRIM(RTOS(XSUM(1,3),'F',3)) 
       ELSE
        LINE=TRIM(LINE)//','//TRIM(RTOS(NODATA_DEFAULT,'F',3))
       ENDIF
      ENDIF
      !## nGXG_IPFA - nGXG_IPFB
      IF(IOPTIONS(IRELATE(14)).EQ.1.AND.IOPTIONS(IRELATE(17)).EQ.1)THEN
       IF(XSUM(1,4).NE.NODATA_DEFAULT)THEN 
        LINE=TRIM(LINE)//','//TRIM(RTOS(XSUM(1,4),'F',3)) 
       ELSE
        LINE=TRIM(LINE)//','//TRIM(RTOS(NODATA_DEFAULT,'F',3))
       ENDIF
      ENDIF

      !## Dynamiek_IPFA - Dynamiek_IPFB
      IF(IOPTIONS(IRELATE(12)).EQ.1.AND.IOPTIONS(IRELATE(15)).EQ.1)THEN
       IF(XSUM(1,5).NE.NODATA_DEFAULT)THEN 
        LINE=TRIM(LINE)//','//TRIM(RTOS(XSUM(1,5),'F',3)) 
       ELSE
        LINE=TRIM(LINE)//','//TRIM(RTOS(NODATA_DEFAULT,'F',3))
       ENDIF
      ENDIF

      LINE=TRIM(LINE)//','//TRIM(ITOS(N))

     ENDIF

    ENDDO

    WRITE(IU,'(A)') TRIM(LINE)
    CALL UTL_WAITMESSAGE(IRAT,IRAT1,J,NU,'Progress Filling Polygons')

   ENDDO
  ENDIF
  LEX=.TRUE.
 ELSE
  LEX=.FALSE.
 ENDIF
 
 IF(ALLOCATED(XRESULT))DEALLOCATE(XRESULT)
 IF(ALLOCATED(XSUM))   DEALLOCATE(XSUM)
 IF(ALLOCATED(IPOL))   DEALLOCATE(IPOL)
 CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 
 !## close result file
 CLOSE(IU)

 !## clean memory
 CALL TSCLEAN()

 CALL IPFDEALLOCATE()
 IF(IGEN.EQ.1)THEN
  CALL POLYGON1CLOSE()
  DEALLOCATE(XPERC)
 ENDIF

 CALL UTL_MESSAGEHANDLE(1)

 IF(LEX.AND.EXITCODE.EQ.0)THEN
  IF(IBATCH.EQ.0)THEN
   CALL WDIALOGGETSTRING(IDF_STRING5,OUTNAME)
   CALL IDFINIT(IDFNAMEGIVEN=OUTNAME,LPLOT=.TRUE.)
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully created '//CHAR(13)//TRIM(OUTNAME)//CHAR(13)//&
   'File has been added to the iMOD-Manager','Information')
  ELSE
   WRITE(*,'(A)') 'Successfully created '//TRIM(OUTNAME)
  ENDIF
 ENDIF

 END SUBROUTINE TSTAT1APPLY

 !###======================================================================
 LOGICAL FUNCTION TSTAT1UTL_INSIDEPOLYGON(X,Y,IPOL)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: X,Y
 INTEGER,INTENT(OUT) :: IPOL

 TSTAT1UTL_INSIDEPOLYGON=.TRUE. !## check polygon ...
 IF(SHPNO.GT.0)THEN
  DO IPOL=1,SHPNO
   IF(X.GT.MINVAL(SHPXC(1:SHPNCRD(IPOL),IPOL)).AND.X.LT.MAXVAL(SHPXC(1:SHPNCRD(IPOL),IPOL)).AND. &
      Y.GT.MINVAL(SHPYC(1:SHPNCRD(IPOL),IPOL)).AND.Y.LT.MAXVAL(SHPYC(1:SHPNCRD(IPOL),IPOL)))THEN
    IF(UTL_INSIDEPOLYGON(X,Y,SHPXC(:,IPOL),SHPYC(:,IPOL),SHPNCRD(IPOL)).EQ.1)EXIT
   ENDIF
  ENDDO
  IF(IPOL.GT.SHPNO)TSTAT1UTL_INSIDEPOLYGON=.FALSE.
 ENDIF

 END FUNCTION TSTAT1UTL_INSIDEPOLYGON

 !###======================================================================
 SUBROUTINE TSTAT1GETDATES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IU,IIPF,JIPF,MINJ,MAXJ,IY,IM,ID
 LOGICAL :: LEX

 CALL UTL_MESSAGEHANDLE(0)

 IF(TSTAT1READIPFS(IIPF,JIPF,0))THEN

  !## allocate memory for associated file object
  CALL IPFASSFILEALLOCATE(1)

  MINJ= 10E8
  MAXJ=-10E8
  !## start processing each of the records ... ipf A or ipf B (iipf-value)
  DO I=1,IPF(IIPF)%NROW

   !## both available
   IF(IIPF+JIPF.EQ.3)THEN
    FNAME1=TRIM(DIR1)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//'.'//TRIM(IPF(IIPF)%FEXT)
!    FNAME2=TRIM(DIR2)//'\'//TRIM(IPF(JIPF)%INFO(IPF(JIPF)%ACOL,I))//'.'//TRIM(IPF(JIPF)%FEXT)
   ELSE
    IF(IIPF.EQ.1)FNAME1=TRIM(DIR1)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//'.'//TRIM(IPF(IIPF)%FEXT)
    IF(IIPF.EQ.2)FNAME2=TRIM(DIR2)//'\'//TRIM(IPF(IIPF)%INFO(IPF(IIPF)%ACOL,I))//'.'//TRIM(IPF(IIPF)%FEXT)
   ENDIF

   !## read associated files
   IF(LEN_TRIM(FNAME1).GT.0)THEN
    IF(IPFOPENASSFILE(IU,1,FNAME1).AND. &
       IPFREADASSFILELABEL(IU,1,FNAME1).AND.  &
       IPFREADASSFILE(IU,1,FNAME1))THEN
     DO J=1,ASSF(1)%NRASS
      MINJ=MIN(MINJ,INT(ASSF(1)%IDATE(J)))
      MAXJ=MAX(MAXJ,INT(ASSF(1)%IDATE(J)))
     ENDDO
    ELSE
     EXIT
    ENDIF
    INQUIRE(UNIT=IU,OPENED=LEX)
    IF(LEX)CLOSE(IU)
   ENDIF

  ENDDO
  !## clean memory
  CALL IPFCLOSEASSFILE()
  CALL UTL_GDATE(MINJ,IY,IM,ID)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER1,ID)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER2,IY)
  CALL WDIALOGPUTOPTION(IDF_MENU2,IM)
  CALL UTL_GDATE(MAXJ,IY,IM,ID)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER3,ID)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER4,IY)
  CALL WDIALOGPUTOPTION(IDF_MENU3,IM)
 ENDIF

 !## clean memory
 CALL IPFDEALLOCATE()

 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE TSTAT1GETDATES

 !###======================================================================
 LOGICAL FUNCTION TSTAT1READIPFS(IIPF,JIPF,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER,INTENT(OUT) :: IIPF,JIPF
 INTEGER :: I,J
 LOGICAL :: LEX

 TSTAT1READIPFS=.FALSE.

 !## two ipf ... read into memory (possible)
 NIPF=2
 CALL IPFALLOCATE()

 DIR1  =''
 DIR2  =''
 IF(IBATCH.EQ.0)THEN
  IPFNAME1=''
  IPFNAME2=''
 ENDIF
 LEX   =.TRUE.
 IIPF  =0
 JIPF  =0

 !## ipf A
 I=1
 IF(IBATCH.EQ.0)CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 IF(I.EQ.1)THEN
  IF(IBATCH.EQ.0)CALL WDIALOGGETSTRING(IDF_STRING1,IPFNAME1)
  IF(LEN_TRIM(IPFNAME1).EQ.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Give in a name for IPF (A)','Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error: Give in a name for IPF (A)'
   RETURN
  ENDIF
  DIR1        =IPFNAME1(:INDEX(IPFNAME1,'\',.TRUE.)-1)
  IPF(1)%FNAME=IPFNAME1
  IPF(1)%XCOL =1
  IPF(1)%YCOL =2
  IPF(1)%ZCOL =2
  IPF(1)%Z2COL=2
  IPF(1)%QCOL =2
  IIPF=1
  IF(.NOT.IPFREAD2(1,1,1))RETURN
  IF(IPF(1)%ACOL.LE.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No associated files found for IPF (A)'//CHAR(13)// &
   '['//TRIM(IPFNAME1)//']','Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error: No associated files found for IPF (A) ['//TRIM(IPFNAME1)//']'
   RETURN
  ENDIF
 ENDIF

 !## ipf B
 IF(IBATCH.EQ.0)CALL WDIALOGGETCHECKBOX(IDF_CHECK2,J)
 IF(IBATCH.EQ.1)J=MIN(LEN_TRIM(IPFNAME2),1)
 IF(J.EQ.1)THEN
  IF(IBATCH.EQ.0)CALL WDIALOGGETSTRING(IDF_STRING2,IPFNAME2)
  IF(LEN_TRIM(IPFNAME2).EQ.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Give in a name for IPF (B)','Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error: Give in a name for IPF (B)'
   RETURN
  ENDIF
  DIR2        =IPFNAME2(:INDEX(IPFNAME2,'\',.TRUE.)-1)
  IPF(2)%FNAME=IPFNAME2
  IPF(2)%XCOL =1
  IPF(2)%YCOL =2
  IPF(2)%ZCOL =2
  IPF(2)%Z2COL=2
  IPF(2)%QCOL =2
  IF(IIPF.EQ.0)IIPF=2
  JIPF=2
  IF(.NOT.IPFREAD2(2,1,1))RETURN
  IF(IPF(2)%ACOL.LE.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No associated files found for IPF (B)'//CHAR(13)// &
   '['//TRIM(IPFNAME2)//']','Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error: No associated files found for IPF (B) ['//TRIM(IPFNAME2)//']'
   RETURN
  ENDIF
 ENDIF

 TSTAT1READIPFS=.TRUE.

 END FUNCTION TSTAT1READIPFS

 !###======================================================================
 LOGICAL FUNCTION TSTAT1RELATE(IIPF,JIPF,IROW,JROW,IC1,IC2)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IC1,IC2,IROW,IIPF,JIPF
 INTEGER,INTENT(INOUT) :: JROW
 INTEGER :: NI

 TSTAT1RELATE=.FALSE.

 NI=1
 DO
  JROW=JROW+1
  IF(JROW.GT.IPF(JIPF)%NROW)THEN
   !## second time to find connection ... does not exist!
   IF(NI.EQ.2)RETURN
   JROW=1
   NI=NI+1
  ENDIF
  IF(TRIM(IPF(IIPF)%INFO(IC1,IROW)).EQ.TRIM(IPF(JIPF)%INFO(IC2,JROW)))EXIT
 ENDDO

 TSTAT1RELATE=.TRUE.

 END FUNCTION TSTAT1RELATE

 !###======================================================================
 SUBROUTINE TSTAT1FIELDS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IOK

 !## one ipf should be selected anyhow!
 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,J)
 IOK=MIN(1,I+J)

 !## ipf (a)
 IF(WINFODIALOGFIELD(IDF_STRING1,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(IDF_STRING1,I)
 IF(WINFODIALOGFIELD(IDF_OPEN1,FIELDSTATE).NE.I)  CALL WDIALOGFIELDSTATE(IDF_OPEN1,I)
 IF(WINFODIALOGFIELD(IDF_MENU4,FIELDSTATE).NE.I)  CALL WDIALOGFIELDSTATE(IDF_MENU4,I)
 IF(WINFODIALOGFIELD(IDF_LABEL6,FIELDSTATE).NE.I) CALL WDIALOGFIELDSTATE(IDF_LABEL6,I)
 CALL WDIALOGGETSTRING(IDF_STRING1,FNAME1)
 IOK=MIN(IOK,TSTAT1STATOK(FNAME1,I,'(A)'))

 !## ipf (b)
 IF(WINFODIALOGFIELD(IDF_STRING2,FIELDSTATE).NE.J)CALL WDIALOGFIELDSTATE(IDF_STRING2,J)
 IF(WINFODIALOGFIELD(IDF_OPEN2,FIELDSTATE).NE.J)  CALL WDIALOGFIELDSTATE(IDF_OPEN2,J)
 IF(WINFODIALOGFIELD(IDF_MENU5,FIELDSTATE).NE.J)  CALL WDIALOGFIELDSTATE(IDF_MENU5,J)
 IF(WINFODIALOGFIELD(IDF_LABEL7,FIELDSTATE).NE.J) CALL WDIALOGFIELDSTATE(IDF_LABEL7,J)
 CALL WDIALOGGETSTRING(IDF_STRING2,FNAME1)
 IF(IOK.EQ.1)IOK=MIN(IOK,TSTAT1STATOK(FNAME1,J,'(B)'))

 CALL TSTAT1GETVAR()

 !## idf
 CALL WDIALOGGETCHECKBOX(IDF_CHECK3,I)
 IF(WINFODIALOGFIELD(IDF_STRING3,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(IDF_STRING3,I)
 IF(WINFODIALOGFIELD(IDF_OPEN3,FIELDSTATE).NE.I)  CALL WDIALOGFIELDSTATE(IDF_OPEN3,I)
 CALL WDIALOGGETSTRING(IDF_STRING3,FNAME1)
 IF(IOK.EQ.1)IOK=MIN(IOK,TSTAT1STATOK(FNAME1,I,'(C)'))

 !## gen
 CALL WDIALOGGETCHECKBOX(IDF_CHECK4,I)
 IF(WINFODIALOGFIELD(IDF_STRING4,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(IDF_STRING4,I)
 IF(WINFODIALOGFIELD(IDF_OPEN4,FIELDSTATE).NE.I)  CALL WDIALOGFIELDSTATE(IDF_OPEN4,I)
 CALL WDIALOGGETSTRING(IDF_STRING4,FNAME1)
 IF(IOK.EQ.1)IOK=MIN(IOK,TSTAT1STATOK(FNAME1,I,'(D)'))

 !## ipf/gen
 IF(I.EQ.0)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL5,'(E) Output (IPF-file)')
  CALL WDIALOGFIELDSTATE(IDF_STRING6,3)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_STRING6,1)
  CALL WDIALOGPUTSTRING(IDF_LABEL5,'(E) Output (GEN-file), agglomerate percentile:')
 ENDIF

 IF(IOK.EQ.1)THEN
  !## variables should be selected (minimal 1)
  IF(ALLOCATED(IVARS))THEN
   IOK=MIN(SUM(IVARS),IOK)
  ENDIF
 ENDIF
 CALL WDIALOGFIELDSTATE(IDOK,IOK)

 END SUBROUTINE TSTAT1FIELDS

 !###======================================================================
 SUBROUTINE TSTAT1GETVAR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 J=0
 IF(NVARS.GT.0)THEN
  CALL WDIALOGGETMENU(IDF_MENU1,IVARS)
  DO I=1,SIZE(IVARS)
   IF(IVARS(I).EQ.1.AND.INDEX(CVARS(I),'GxG').GT.0)THEN
    J=1
    EXIT
   ENDIF
  ENDDO
 ENDIF

 DO I=1,SIZE(ID); CALL WDIALOGFIELDSTATE(ID(I),J); ENDDO
 IF(J.EQ.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,0)

 END SUBROUTINE TSTAT1GETVAR

 !###======================================================================
 SUBROUTINE TSTAT1PUTVARIABLES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K
 CHARACTER(LEN=1),DIMENSION(0:1) :: CAB
 DATA CAB/'B','A'/

 NVARS=0

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK2,J)

 !## nothing selected
 IF(I+J.EQ.0)THEN
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
  CALL TSTAT1FIELDS()
  RETURN
 ENDIF

 NVARS=NVARS+3
 IF(I.EQ.1.AND.J.EQ.1)NVARS=NVARS+2

 IF(ALLOCATED(CVARS))DEALLOCATE(CVARS)
 IF(ALLOCATED(IVARS))DEALLOCATE(IVARS)
 ALLOCATE(CVARS(NVARS),IVARS(NVARS))

 IF(I+J.EQ.1)THEN
  CVARS(1)='Auto-Correlation'
  CVARS(2)='P50'
  CVARS(3)='(n)GxG'
  DO K=1,SIZE(CVARS); CVARS(K)=TRIM(CVARS(K))//' (IPF '//CAB(I)//')'; ENDDO
 ELSE
  CVARS(1)='Cross-Correlation'
  CVARS(2)='P50 (IPF A)'
  CVARS(3)='P50 (IPF B)'
  CVARS(4)='(n)GxG (IPF A)'
  CVARS(5)='(n)GxG (IPF B)'
 ENDIF

 IVARS=0
 CALL WDIALOGPUTMENU(IDF_MENU1,CVARS,NVARS,IVARS)
 CALL TSTAT1FIELDS()

 END SUBROUTINE TSTAT1PUTVARIABLES

 !###======================================================================
 INTEGER FUNCTION TSTAT1STATOK(FNAME1,IACT,CPOS)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME1,CPOS
 INTEGER,INTENT(IN) :: IACT
 LOGICAL :: LEX

 TSTAT1STATOK=1

 IF(IACT.EQ.0)RETURN
 INQUIRE(FILE=FNAME1,EXIST=LEX)
 IF(.NOT.LEX)TSTAT1STATOK=0
 IF(TSTAT1STATOK.EQ.1)CALL WDIALOGPUTSTRING(IDF_LABEL8,'')
 IF(TSTAT1STATOK.EQ.0)CALL WDIALOGPUTSTRING(IDF_LABEL8,'Unknown file given by: '//TRIM(CPOS))

 END FUNCTION TSTAT1STATOK

 !###======================================================================
 SUBROUTINE TSTAT1INIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_TSTAT,2).EQ.1)THEN
  CALL TSTAT1CLOSE()
  RETURN
 ENDIF
 CALL WMENUSETSTATE(ID_TSTAT,2,1)

 CALL WDIALOGLOAD(ID_DTSTAT,ID_DTSTAT)
 CALL WDIALOGTITLE('Time Variant Statistics')

 CALL WDIALOGPUTIMAGE(IDF_OPEN1,ID_ICONOPEN,1) !## ipf1
 CALL WDIALOGPUTIMAGE(IDF_OPEN2,ID_ICONOPEN,1) !## ipf2
 CALL WDIALOGPUTIMAGE(IDF_OPEN3,ID_ICONOPEN,1) !## gen1
 CALL WDIALOGPUTIMAGE(IDF_OPEN4,ID_ICONOPENIDF,1) !## idf1
 CALL WDIALOGPUTIMAGE(IDF_OPEN5,ID_ICONOPEN,1) !## ipf/gen

 !## string editable, changes will be monitored directly
 CALL WDIALOGFIELDOPTIONS(IDF_STRING1,EDITFIELDCHANGED,ENABLED)
 CALL WDIALOGFIELDOPTIONS(IDF_STRING2,EDITFIELDCHANGED,ENABLED)
 CALL WDIALOGFIELDOPTIONS(IDF_STRING3,EDITFIELDCHANGED,ENABLED)
 CALL WDIALOGFIELDOPTIONS(IDF_STRING4,EDITFIELDCHANGED,ENABLED)
 CALL WDIALOGFIELDOPTIONS(IDF_STRING5,EDITFIELDCHANGED,ENABLED)

 CALL WDIALOGPUTMENU(IDF_MENU2,CDATE,12,3)  !## begindate
 CALL WDIALOGPUTMENU(IDF_MENU3,CDATE,12,1)  !## enddate
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,8)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER2,1970)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER3,1)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER4,2011)

 !CALL WDIALOGPUTSTRING(IDF_STRING1,'D:\TESTS\ipfs\TEST.IPF') !meetreeksen.ipf')
 !CALL TSTAT1READATTRIBUTES(IDF_OPEN1,'D:\TESTS\ipfs\TEST.IPF') !meetreeksen.ipf')
 !CALL WDIALOGPUTSTRING(IDF_STRING2,'D:\TESTS\ipfs\model20gwl.ipf')
 !CALL TSTAT1READATTRIBUTES(IDF_OPEN2,'D:\TESTS\ipfs\model20gwl.ipf')
 !CALL WDIALOGPUTSTRING(IDF_STRING4,'d:\tests\ipfs\NEDERLAND.gen') !deelgeb.gen')
 !CALL WDIALOGPUTSTRING(IDF_STRING5,'d:\tests\ipfs\NEDERLAND_test.gen') !summary.gen')

 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,0)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,0)

 CALL TSTAT1PUTVARIABLES()
! CALL TSTAT1FIELDS()

 !## modelless dialog
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE TSTAT1INIT

 !###======================================================================
 SUBROUTINE TSTAT1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(CVARS))DEALLOCATE(CVARS)
 IF(ALLOCATED(IVARS))DEALLOCATE(IVARS)
 NVARS=0

 CALL WINDOWSELECT(0)
 CALL WMENUSETSTATE(ID_TSTAT,2,0)

 CALL WDIALOGSELECT(ID_DTSTAT)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE TSTAT1CLOSE

END MODULE MOD_TSTAT
