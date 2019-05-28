!!  Copyright (C) Stichting Deltares, 2005-2019.
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

USE IMODVAR, ONLY : SP_KIND,DP_KIND
USE MOD_UTL
USE MOD_TSTAT_PAR
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF
USE MOD_IPF_PAR, ONLY : IPF,NIPF,ASSF
USE MOD_IPF, ONLY : IPFALLOCATE,IPFREAD2,IPFDEALLOCATE
USE MOD_IPFASSFILE, ONLY : IPFOPENASSFILE,IPFREADASSFILELABEL,IPFREADASSFILE,IPFASSFILEALLOCATE,ASSF
USE MOD_IPFASSFILE_UTL

CONTAINS

 !###======================================================================
 SUBROUTINE TSTATRESIDUAL()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: ROOT,FNAME
 INTEGER :: II,I,J,JJ,IU,JU,KU,IOS,ILAY,N,IL,IL1,IL2,NPOP,IZ,NZ,IROW,ICOL
 REAL(KIND=DP_KIND) :: M,H,W,VAR,MEAN,XC,YC,MH,MM,STVDH,MDH,XCOR,YCOR,ZCOR ,DYNH,DYNM 
 REAL(KIND=DP_KIND),DIMENSION(11) :: PERC,XPERC
 REAL(KIND=DP_KIND),DIMENSION(17) :: XHIST
 DATA PERC/0.0D0,10.0D0,20.0D0,30.0D0,40.0D0,50.0D0,60.0D0,70.0D0,80.0D0,90.0D0,100.0D0/
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: X,XW
 REAL(KIND=DP_KIND),DIMENSION(2) :: XMED
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
     IF(W_TYPE(II).EQ.0)IPF(1)%XYZ(3,I)=1.0D0
     
     READ(IPF(1)%INFO(1,I),*) XC; READ(IPF(1)%INFO(2,I),*) YC
     !## check whether insize appropriate zone
     IF(NZONE.GT.0)THEN
      CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
      IF(IDF%X(ICOL,IROW).NE.IZONE(IZ))CYCLE
     ENDIF   

     W=IPF(1)%XYZ(3,I); IF(W.GT.0.0D0.AND.W_TYPE(II).EQ.2)W=1.0D0/SQRT(W); W=MAX(0.0D0,W)
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

     WRITE(6,'(A,F10.2)') '+Progress ',REAL(I)*100.0D0/REAL(IPF(1)%NROW)  

    ENDDO
   
   ELSE
    CALL IPFASSFILEALLOCATE(1)
    DO I=1,IPF(1)%NROW

     !## overrule weigh if not specified
     IF(W_TYPE(II).EQ.0)IPF(1)%XYZ(3,I)=1.0D0

     READ(IPF(1)%INFO(1,I),*) XC; READ(IPF(1)%INFO(2,I),*) YC
     !## check whether inside appropriate zone
     IF(NZONE.GT.0)THEN
      CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
      IF(IDF%X(ICOL,IROW).NE.IZONE(IZ))CYCLE
     ENDIF   
     FNAME=TRIM(ROOT)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,I))//'.'//TRIM(IPF(1)%FEXT)
     W=IPF(1)%XYZ(3,I); IF(W.GT.0.0D0.AND.W_TYPE(II).EQ.2)W=1.0D0/SQRT(W); W=MAX(0.0D0,W)
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
      CALL UTL_GETMED(X,JJ,ASSF(1)%NODATA(3),(/10.0D0,90.0D0/),2,NPOP,XMED); DYNH=XMED(2)-XMED(1)

      !## get dynamic model
      JJ=0; DO J=1,ASSF(1)%NRASS
       M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)   
       IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
        IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN; JJ=JJ+1; X(JJ)=M; ENDIF      
       ENDIF
      ENDDO
      CALL UTL_GETMED(X,JJ,ASSF(1)%NODATA(3),(/10.0D0,90.0D0/),2,NPOP,XMED); DYNM=XMED(2)-XMED(1)

      !## get mean
      JJ=0; MH=0.0D0; MM=0.0D0
      DO J=1,ASSF(1)%NRASS
       M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)
       IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
        IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN; MH=MH+H; MM=MM+M; JJ=JJ+1; ENDIF      
       ENDIF
      ENDDO

      XCOR=0.0D0; YCOR=0.0D0; ZCOR=0.0D0

      IF(JJ.GT.0)THEN
       MH=MH/REAL(JJ); MM=MM/REAL(JJ)

       !## compute cross-correlation
       IF(JJ.GT.1)THEN
        DO J=1,ASSF(1)%NRASS
         M=ASSF(1)%MEASURE(1,J); H=ASSF(1)%MEASURE(2,J)   
         IF(ASSF(1)%IDATE(J).GE.SOBSDATE.AND.ASSF(1)%IDATE(J).LE.EOBSDATE)THEN
          IF(H.NE.ASSF(1)%NODATA(3).AND.M.NE.ASSF(1)%NODATA(2))THEN
           XCOR=XCOR+(MM-M)*(MH-H)
           YCOR=YCOR+(MM-M)**2.0D0
           ZCOR=ZCOR+(MH-H)**2.0D0
          ENDIF
         ENDIF
        ENDDO
        IF(YCOR.NE.0.0D0.AND.ZCOR.NE.0.0D0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
       ENDIF
       
       IF(W.GT.0.0D0)THEN
        N  =N+1
        RES(N)%M   =   STVDH 
        RES(N)%H   =   DYNH 
        RES(N)%DH  =   MDH
        RES(N)%DHW =   DYNH-DYNM
        RES(N)%DHW=0.0D0
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

     WRITE(6,'(A,F10.2)') '+Progress ',REAL(I)*100.0D0/REAL(IPF(1)%NROW)  

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
     CALL UTL_STDEF(X,II,10.0D10,VAR,MEAN,NPOP)
     CALL UTL_GETMED(X,II,10.0D10,PERC,SIZE(PERC),JJ,XPERC)
    ELSE
     XPERC=0.0D0;MEAN=0.0D0; VAR=0.0D0; XHIST=0.0D0
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
   CALL UTL_STDEF(X,N,10.0D10,VAR,MEAN,NPOP)
   CALL UTL_GETMED(X,N,10.0D10,PERC,SIZE(PERC),JJ,XPERC)
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
 
END MODULE MOD_TSTAT
