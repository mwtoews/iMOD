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
MODULE MOD_SOBEK

USE WINTERACTER
USE RESOURCE
USE MOD_SOBEK_PAR  
USE MOD_UTL, ONLY : UTL_WSELECTFILE

USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_CAP,UTL_JDATETOIDATE,UTL_CREATEDIR 
USE IMODVAR, ONLY : PI
USE MOD_QKSORT, ONLY : UTL_QKSORT2

INTEGER,PARAMETER :: ISOBEK=1

CONTAINS

 !###======================================================================
 SUBROUTINE SOBEK1MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 CHARACTER(LEN=256) :: FNAME
 
 CALL WDIALOGLOAD(ID_DIMPORTSOBEK,ID_DIMPORTSOBEK)

 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONSAVEAS)
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_OPEN3,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_OPEN4,ID_ICONOPEN)  

 CALL WDIALOGSELECT(ID_DIMPORTSOBEK)
 CALL WDIALOGSHOW(-1,-1,0,3)
 IBATCH=0
 
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)

   CASE (TABCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
    END SELECT

   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
    END SELECT

   CASE(PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_OPEN1)
      FNAME='' 
      IF(UTL_WSELECTFILE('iMOD ISG file|*.isg|', &
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save iMOD ISG File (*.isg)'))THEN
       CALL WDIALOGPUTSTRING(IDF_STRING1,FNAME)
      ENDIF
     CASE (ID_OPEN2)
      FNAME='' 
      IF(UTL_WSELECTFILE('Sobek Network File|*.tp|', &
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Load Sobek Network File (network.tp)'))THEN
       CALL WDIALOGPUTSTRING(IDF_STRING2,FNAME)
      ENDIF
     CASE (ID_OPEN3)
      FNAME='' 
      IF(UTL_WSELECTFILE('SOBEK Calcpnt His-file|*.his|', &
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Load SOBEK His file (*.his)'))THEN
       CALL WDIALOGPUTSTRING(IDF_STRING3,FNAME)
      ENDIF
     CASE (ID_OPEN4)
      FNAME='' 
      IF(UTL_WSELECTFILE('SOBEK Struc His-file|*.his|', &
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Load SOBEK His file (*.his)'))THEN
       CALL WDIALOGPUTSTRING(IDF_STRING4,FNAME)
      ENDIF
     CASE (IDOK)
      CALL SOBEK1FIELDS
      IF(SOBEK1CALC())EXIT
     CASE (IDHELP)
       CALL IMODGETHELP('5.5.1','TMO.IT.SOBEK')
     !## cancel modeling
     CASE (IDCANCEL)
      EXIT
    END SELECT

  END SELECT
 ENDDO
 
 CALL WDIALOGSELECT(ID_DIMPORTSOBEK)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE SOBEK1MAIN

 !###======================================================================
 SUBROUTINE SOBEK1FIELDS
 !###======================================================================
 IMPLICIT NONE
 
 CALL WDIALOGGETSTRING(IDF_STRING1,ISGNAME)
 CALL WDIALOGGETSTRING(IDF_STRING2,NETWORKNAME)
 CALL WDIALOGGETSTRING(IDF_STRING3,CALCPNTHISNAME)
 CALL WDIALOGGETSTRING(IDF_STRING4,STRUCHISNAME)

 END SUBROUTINE SOBEK1FIELDS

 !###======================================================================
 LOGICAL FUNCTION SOBEK1CALC()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 LOGICAL :: LEX

 SOBEK1CALC=.FALSE.

 FNAME(IOUT)=TRIM(PREFVAL(1))//'\tmp\imod2sobek.log'  
 IU(IOUT)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(IOUT),FILE=FNAME(IOUT),STATUS='UNKNOWN',FORM='FORMATTED',IOSTAT=IOS(IOUT))
 
 I=INDEX(NETWORKNAME,'\',.TRUE.)

 !## input
 FNAME(ITP)= NETWORKNAME(:I)//'NETWORK.TP'
 FNAME(ICR)= NETWORKNAME(:I)//'NETWORK.CR'
 FNAME(ICP)= NETWORKNAME(:I)//'NETWORK.CP'
 FNAME(IGR)= NETWORKNAME(:I)//'NETWORK.GR'
 FNAME(IST)= NETWORKNAME(:I)//'NETWORK.ST'
 FNAME(IAT)= NETWORKNAME(:I)//'PROFILE.DAT'
 FNAME(IEF)= NETWORKNAME(:I)//'PROFILE.DEF'
 FNAME(IFR)= NETWORKNAME(:I)//'FRICTION.DAT'
 FNAME(IHIS)=CALCPNTHISNAME 
 FNAME(SHIS)=STRUCHISNAME   
 
 WRITE(IU(IOUT),'(A)') ' Reads:   '//TRIM(FNAME(ITP)) !NETWORK.TP'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ICR)) !NETWORK.CR'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ICP)) !NETWORK.CP'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IGR)) !NETWORK.GR'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IST)) !NETWORK.ST'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IAT)) !PROFILE.DAT'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IEF)) !PROFILE.DEF'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IFR)) !FRICTION.DAT'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IHIS))!CLCPNT.HIS'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(SHIS))!STRUC.HIS'

 !## create folder
 I=INDEX(ISGNAME,'\',.TRUE.)-1
 CALL UTL_CREATEDIR(ISGNAME(:I))

 I=INDEX(ISGNAME,'.',.TRUE.)-1
 IF(I.EQ.-1)I=LEN_TRIM(ISGNAME) !## no extention found
  
 FNAME(ISG) =ISGNAME(:I)//'.ISG'
 FNAME(ISP) =ISGNAME(:I)//'.ISP'
 FNAME(ISD1)=ISGNAME(:I)//'.ISD1'
 FNAME(ISD2)=ISGNAME(:I)//'.ISD2'
 FNAME(ISC1)=ISGNAME(:I)//'.ISC1'
 FNAME(ISC2)=ISGNAME(:I)//'.ISC2'
 FNAME(IST1)=ISGNAME(:I)//'.IST1'
 FNAME(IST2)=ISGNAME(:I)//'.IST2'
 FNAME(ISQ1)=ISGNAME(:I)//'.ISQ1'
 FNAME(ISQ2)=ISGNAME(:I)//'.ISQ2'
 
 WRITE(IU(IOUT),'(A)') ' Creates: '//TRIM(FNAME(ISG)) !IMOD.ISG'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ISP)) !IMOD.ISP'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ISD1))!IMOD.ISD1'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ISD2))!IMOD.ISD2'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ISC1))!IMOD.ISC1'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ISC2))!IMOD.ISC2'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IST1))!IMOD.IST1'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(IST2))!IMOD.IST2'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ISQ1))!IMOD.ISQ1'
 WRITE(IU(IOUT),'(A)') '          '//TRIM(FNAME(ISQ2))!IMOD.ISQ2'
 WRITE(IU(IOUT),'(A/)') '          '//TRIM(FNAME(IOUT))!LOG-FILE'
 
 IOS=0

 DO J=1,8
  I=IL(J)
  IU(I)=UTL_GETUNIT()
  CALL OSD_OPEN(IU(I),FILE=FNAME(I),STATUS='OLD',ACTION='READ',IOSTAT=IOS(I))
  !## error opening file
  IF(IOS(I).NE.0)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find:'//CHAR(13)//TRIM(FNAME(I)),'Error')
   ELSE
    WRITE(*,'(A)') 'Can not find:'//CHAR(13)//TRIM(FNAME(I))
   ENDIF
   RETURN
  ENDIF
 END DO

 IU(ISG)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISG),FILE=FNAME(ISG),STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS(ISG))
 IU(ISP)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISP),FILE=FNAME(ISP),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT'  ,RECL=2,IOSTAT=IOS(ISP))
 IU(ISD1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISD1),FILE=FNAME(ISD1),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=11,IOSTAT=IOS(ISD1))
 IU(ISD2)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISD2),FILE=FNAME(ISD2),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=5,IOSTAT=IOS(ISD2))
 IU(ISC1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISC1),FILE=FNAME(ISC1),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=11,IOSTAT=IOS(ISC1))
 IU(ISC2)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISC2),FILE=FNAME(ISC2),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=3,IOSTAT=IOS(ISC2))
 IU(IST1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(IST1),FILE=FNAME(IST1),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=11,IOSTAT=IOS(IST1))
 IU(IST2)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(IST2),FILE=FNAME(IST2),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=3,IOSTAT=IOS(IST2))
 IU(ISQ1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISQ1),FILE=FNAME(ISQ1),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=11,IOSTAT=IOS(ISQ1))
 IU(ISQ2)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(ISQ2),FILE=FNAME(ISQ2),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4,IOSTAT=IOS(ISQ2))
 
 DO I=8,18
  IF(IOS(I).NE.0)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create:'//CHAR(13)//TRIM(FNAME(I)),'Error')
   ELSE
    WRITE(*,'(A)') 'Can not create:'//CHAR(13)//TRIM(FNAME(I))
   ENDIF
   RETURN
  ENDIF
 ENDDO
 
 !## get his in data set
 IOS(IHIS)=0
 IF(IBATCH.EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(FNAME(IHIS))//'...')
 ELSE
  WRITE(*,'(/1X,A)') 'Reading '//TRIM(FNAME(IHIS))//'...'
 ENDIF
 HISDATASET=DIOPLTGETDATASET(FNAME(IHIS))
 IF(.NOT.DIOPLTOPENEDOK(HISDATASET))THEN
  CALL DIOPLTDESTROY(HISDATASET)
  IOS(IHIS)=-1
 ENDIF
 IF(FNAME(SHIS).NE.'')THEN
  IF(IBATCH.EQ.0)THEN
   CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(FNAME(SHIS))//'...')
  ELSE
   WRITE(*,'(/1X,A)') 'Reading '//TRIM(FNAME(SHIS))//'...'
  ENDIF
  SHISDATASET=DIOPLTGETDATASET(FNAME(SHIS))
  IF(.NOT.DIOPLTOPENEDOK(SHISDATASET))THEN
   CALL DIOPLTDESTROY(SHISDATASET)
   IOS(SHIS)=-1
  ENDIF
 ENDIF
 
 DIMTIMES=0

 IF(SUM(IOS).EQ.0)CALL MAIN()

 IF(IBATCH.NE.0)WRITE(*,*)
 DO I=1,NIU
  IF(IOS(I).EQ.0)THEN
   INQUIRE(UNIT=IU(I),OPENED=LEX)
   IF(LEX)CLOSE(IU(I))
  ELSE
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open/find: '//TRIM(FNAME(I)),'Error')
   ELSE
    WRITE(*,'(1X,A)') 'Can not open/find: '//TRIM(FNAME(I))
   ENDIF
  ENDIF
 END DO
 !## destroy dataset
 IF(IOS(IHIS).EQ.0)CALL DIOPLTDESTROY(HISDATASET)
 
 CLOSE(IU(IOUT))
 
 IF(SUM(IOS).EQ.0)THEN
  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Sobek Network and results from selected *his file(s):'//CHAR(13)// &
    TRIM(FNAME(IHIS))//CHAR(13)//TRIM(FNAME(SHIS))//CHAR(13)//'Successfully imported to iMOD-isg:'//CHAR(13)// &
    TRIM(FNAME(ISG)),'Information')
  ELSE
   WRITE(*,'(/1X,A/)') ' Program Successfully completed '
  ENDIF
 ENDIF
 
 SOBEK1CALC=.TRUE.

 END FUNCTION SOBEK1CALC

 !###======================================================================
 SUBROUTINE MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 !## allocate memory for SOBEK-system
 IF(IBATCH.EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(4,'Allocated ...')
 ELSE
  WRITE(*,'(1X,A)') 'Allocated ...'
 ENDIF

 CALL MAIN1ALNETWORKTP()
 CALL MAIN1ALNETWORKCP()
 CALL MAIN1ALNETWORKCR()
 CALL MAIN1ALNETWORKGR()
 CALL MAIN1ALNETWORKST()
 CALL MAIN1ALPROFILEDAT()
 CALL MAIN1ALPROFILEDEF()
 CALL MAIN1ALFRICTIONDAT()

 ALLOCATE(TP1(NNODE),TP2(NBRCH),TP3(NNDLK),CP(NCPNT),CR(NCROS),GR(NCALCP), &
          PDAT(NDAT),PDEF(NDEF),BDFR(NBDFR),ST(NSTUW),QH(NCALCP))
 DO I=1,SIZE(PDEF); NULLIFY(PDEF(I)%XPROF,PDEF(I)%YPROF); ENDDO
 
 IF(IBATCH.EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(4,'Allocated ...')
 ELSE
  WRITE(*,'(1X,A)') 'Read ...'
 ENDIF
 
 CALL MAIN1RDNETWORKTP()
 CALL MAIN1RDNETWORKCP()
 CALL MAIN1RDNETWORKCR()
 CALL MAIN1RDNETWORKGR()
 CALL MAIN1RDNETWORKST()
 CALL MAIN1RDPROFILEDAT()
 CALL MAIN1RDPROFILEDEF()
 CALL MAIN1RDFRICTIONDAT()

 CALL ISGCREATE()

 DO I=1,SIZE(PDEF)
  IF(ASSOCIATED(PDEF(I)%XPROF))DEALLOCATE(PDEF(I)%XPROF)
  IF(ASSOCIATED(PDEF(I)%YPROF))DEALLOCATE(PDEF(I)%YPROF)
 ENDDO
 
 DEALLOCATE(TP1,TP2,CP,CR,GR,PDAT,PDEF,BDFR,ST,QH)

 END SUBROUTINE MAIN

 !##=====================================================================
 SUBROUTINE ISGCREATE()
 !##=====================================================================
 IMPLICIT NONE
 INTEGER :: ISEG,ICRS,IB,NNP,ICLC,NSEG,NCLC,NCRS,IREFSD,IREFSC,IREFST, &
            NSTW,ISTW,NQHR,IQHR,IREFSQ

 ALLOCATE(X(MAXCPNT),Y(MAXCPNT))

 WRITE(IU(ISG),*) NBRCH

 IF(IBATCH.EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(4,'')
 ELSE
  WRITE(*,'(/A/)')
 ENDIF
 
 ISEG  =0  !## segment-points
 ICRS  =0  !## cross-sections
 ICLC  =0  !## data
 ISTW  =0
 IQHR  =0
 IREFSD=0
 IREFSC=0
 IREFST=0
 IREFSQ=0
 NNP   =0
 DO IB=1,NBRCH

  IF(.NOT.ISPCREATE(IB,ISEG,NNP,NSEG))EXIT
  CALL ISD1CREATE(ICLC,NCLC,IREFSD,IB)
  CALL ISC1CREATE(ICRS,IB,NCRS,IREFSC)
  CALL IST1CREATE(ISTW,IB,NSTW,IREFST)
  CALL ISQ1CREATE(IQHR,IB,NQHR,IREFSQ)

  LINE='"'//TRIM(TP2(IB)%ID)//'",'//TRIM(ITOS(ISEG-NSEG+1))//','//TRIM(ITOS(NSEG))//','// &
                                    TRIM(ITOS(ICLC-NCLC+1))//','//TRIM(ITOS(NCLC))//','// &
                                    TRIM(ITOS(ICRS-NCRS+1))//','//TRIM(ITOS(NCRS))//','// &
                                    TRIM(ITOS(ISTW-NSTW+1))//','//TRIM(ITOS(NSTW))//','// &
                                    TRIM(ITOS(IQHR-NQHR+1))//','//TRIM(ITOS(NQHR))
  WRITE(IU(ISG),*) TRIM(LINE)

  IF(IBATCH.EQ.0)THEN
   CALL WINDOWOUTSTATUSBAR(4,'Progress '//TRIM(RTOS(REAL(IB*100)/REAL(NBRCH),'F',2))//' %')
  ELSE
   WRITE(*,'(1X,A,F10.4,A)') 'Progress ',REAL(IB*100)/REAL(NBRCH),'%         '
  ENDIF
  
 END DO

 WRITE(IU(ISP) ,REC=1)  8*256+247
 WRITE(IU(ISD1),REC=1) 44*256+247 
 WRITE(IU(ISD2),REC=1) 20*256+247
 WRITE(IU(ISC1),REC=1) 44*256+247 
 WRITE(IU(ISC2),REC=1) 12*256+247
 WRITE(IU(IST1),REC=1) 44*256+247 
 WRITE(IU(IST2),REC=1) 12*256+247
 WRITE(IU(ISQ1),REC=1) 44*256+247 
 WRITE(IU(ISQ2),REC=1) 16*256+247

 IF(IBATCH.NE.0)THEN
  WRITE(*,'(A)') 'Number of records in:'
  WRITE(*,'(A,I10)') TRIM(FNAME(ISG))  //' ',NBRCH
  WRITE(*,'(A,I10)') TRIM(FNAME(ISP))  //' ',NNP
  WRITE(*,'(A,2I10)') TRIM(FNAME(ISD1))//' ',ICLC,IREFSD
  WRITE(*,'(A,2I10)') TRIM(FNAME(ISC1))//' ',ICRS,IREFSC
  WRITE(*,'(A,2I10)') TRIM(FNAME(IST1))//' ',ISTW,IREFST
  WRITE(*,'(A,2I10)') TRIM(FNAME(ISQ1))//' ',IQHR,IREFSQ
 ENDIF
 
 IF(ALLOCATED(X))DEALLOCATE(X)
 IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(TIMINDICES))DEALLOCATE(TIMINDICES)
 IF(ALLOCATED(JULIANTIMES))DEALLOCATE(JULIANTIMES)
 IF(ALLOCATED(SELECTEDVALUES))DEALLOCATE(SELECTEDVALUES)

 END SUBROUTINE ISGCREATE

 !##=====================================================================
 LOGICAL FUNCTION ISPCREATE(IB,ISEG,NNP,NSEG)
 !##=====================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: RAD=360.0/(2.0*3.1415926)
 INTEGER,INTENT(IN) :: IB
 INTEGER,INTENT(INOUT) :: ISEG,NNP
 INTEGER,INTENT(OUT) :: NSEG
 REAL :: DIST,D
 DOUBLE PRECISION :: XC,YC
 INTEGER :: I,J,K

 ISPCREATE=.FALSE.
 
 !#begin node
 J=1
 
 DO I=1,NNODE
  IF(TRIM(TP1(I)%ID).EQ.TRIM(TP2(IB)%CBN))EXIT 
 END DO
 IF(I.LE.NNODE)THEN
  XC=TP1(I)%PX; YC=TP1(I)%PY
 ELSE 
  !## try from flow-linkage nodes, if available
  DO I=1,NNDLK
   IF(TRIM(TP3(I)%ID).EQ.TRIM(TP2(IB)%CBN))EXIT 
  END DO
  IF(I.LE.NNDLK)THEN
   XC=TP3(I)%PX; YC=TP3(I)%PY
  ELSE
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not match identification ('//TRIM(TP2(IB)%CBN)// &
    ') for branch: '//TRIM(ITOS(IB)),'Error')
   IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'Can not match identification ('//TRIM(TP2(IB)%CBN)//') for branch: ',IB
   RETURN
  ENDIF
 ENDIF
 X(J)=XC; Y(J)=YC

 !## put in between curving points
 D=0.0
 DO I=1,NCPNT
 !## curving points id equal to current branch-id
  IF(TRIM(CP(I)%ID).EQ.TRIM(TP2(IB)%ID))THEN

   !## angle->radians
   DIST=CP(I)%SLEN-D
   D   =D+DIST
   XC  =XC+SIN(CP(I)%ANGLE/RAD)*DIST
   YC  =YC+COS(CP(I)%ANGLE/RAD)*DIST
   J   =J+1
   IF(J.GT.MAXCPNT)THEN
    STOP 'increase MAXCPNT'
   ENDIF
   X(J)=XC
   Y(J)=YC

  ENDIF
 END DO

 !## skip double points
 K=1
 DO I=2,J
  IF(REAL(X(I)).NE.REAL(X(I-1)).OR.REAL(Y(I)).NE.REAL(Y(I-1)))THEN
   K   =K+1
   X(K)=X(I)
   Y(K)=Y(I)
  ENDIF
 END DO

 J=K

 DIST=0.0
 DO I=1,J
  IF(I.GT.1)THEN
   IF(X(I).NE.X(I-1).OR.Y(I).NE.Y(I-1))THEN
    DIST=DIST+SQRT((X(I)-X(I-1))**2.0+(Y(I)-Y(I-1))**2.0)
    ISEG=ISEG+1
    WRITE(IU(ISP),REC=ISEG+1) REAL(X(I)),REAL(Y(I))
   ENDIF
  ELSE
   ISEG=ISEG+1
   WRITE(IU(ISP),REC=ISEG+1) REAL(X(I)),REAL(Y(I))
  ENDIF
 END DO

 IF(ABS(TP2(IB)%AL-DIST).GT.1.0)THEN
  WRITE(IU(IOUT),'(2(A,F15.7))') 'Actual length ',TP2(IB)%AL,' not equal to computed length',DIST
 ENDIF
 
 !## adjust correct segment length
 TP2(IB)%AL=DIST

 NSEG=J
 NNP =NNP+J

 ISPCREATE=.TRUE.

 END FUNCTION ISPCREATE

 !##=====================================================================
 SUBROUTINE ISD1CREATE(ICLC,NCLC,IREFSD,IB)
 !##=====================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: NODATA=-999.99
 INTEGER,INTENT(IN) :: IB
 INTEGER,INTENT(INOUT) :: ICLC,IREFSD
 INTEGER,INTENT(OUT) :: NCLC
 INTEGER :: ID,I,J,N
 CHARACTER(LEN=MAXLEN) :: CROSNAME

 !##from zomer/winter
 !CALL ISD1GETDATA('calcpnt.HIS')

 !ID=20040101

 !##from
 !ICLC  =ICLC+1
 !IREFSD=IREFSD+1
 !WRITE(IU(ISD1),REC=ICLC+1) 1,IREFSD,0.0,'CalcPnt_FR'
 !WRITE(IU(ISD2),REC=IREFSD+1) ID,WL(1),WB(1),RS(1),FC(1)

 !##to
 !ICLC  =ICLC+1
 !IREFSD=IREFSD+1
 !WRITE(IU(ISD1),REC=ICLC+1) 1,IREFSD,REAL(TP2(IB)%AL),'CalcPnt_TO'
 !WRITE(IU(ISD2),REC=IREFSD+1) ID,WL(3),WB(3),RS(3),FC(3)

 !NCLC  =2
 !RETURN

 !10 CONTINUE

 NCLC=0
 DO I=1,NCALCP
  IF(TRIM(TP2(IB)%ID).EQ.TRIM(GR(I)%CI))THEN

   !## from his-file
   CALL HIS1GETDATA(GR(I)%ID,1)

   IF(NTIMES.GT.0)THEN

    NCLC    =NCLC+1
    ICLC    =ICLC+1

    N       =NTIMES
    CROSNAME=GR(I)%ID

    WRITE(IU(ISD1),REC=ICLC+1) N,IREFSD+1,REAL(GR(I)%LC),CROSNAME
    DO J=1,N
     IREFSD=IREFSD+1
     ID    =UTL_JDATETOIDATE(INT(JULIANTIMES(J)))
     WRITE(IU(ISD2),REC=IREFSD+1) ID,SELECTEDVALUES(1,1,J),SELECTEDVALUES(2,1,J),1.0,0.3
    END DO

   ENDIF
  ENDIF
 ENDDO

 !IF(NCLC.EQ.0)WRITE(*,*) TRIM(TP2(IB)%ID)//' not found'

 !#check whether last calculationpoint lenth is equal to actual length of segment
 !READ(IU(ISD1),REC=ICLC+1)  N,IREFSD,LENGTH,CROSNAME
 !WRITE(IU(ISD1),REC=ICLC+1) N,IREFSD,REAL(TP2(IB)%AL),CROSNAME

 RETURN
 END SUBROUTINE

 !##=====================================================================
 SUBROUTINE HIS1GETDATA(CLCID,ITYPE)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CLCID
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER :: NPAR,NLOC
 CHARACTER(LEN=DIOMAXPARLEN),DIMENSION(:),POINTER :: PARS
 CHARACTER(LEN=DIOMAXLOCLEN),DIMENSION(:),POINTER :: LOCS
 !CHARACTER(LEN=30),DIMENSION(:),POINTER :: LOCS
 INTEGER,DIMENSION(2) :: WATERLEVELINDICES
 INTEGER,DIMENSION(1) :: SELECTEDLOCATIONINDICES
 !CHARACTER(LEN=DIOMAXPARLEN),DIMENSION(5,2) :: QUANTITYID
 INTEGER :: I
 LOGICAL :: LWP,LWB
  
 !QUANTITYID(1,1)='Waterlevel  (m AD)'
 !QUANTITYID(2,1)='Waterdepth  (m)'
 !QUANTITYID(1,2)='Discharge (m|/s)'
 !QUANTITYID(2,2)='Waterlevel up (m AD)'
 !QUANTITYID(3,2)='Waterlevel down (m AD)'
 !QUANTITYID(4,2)='Crest level (m AD)'
 !QUANTITYID(5,2)='Gate lower edge level (m AD)'

 ! find the index of the requested quantity
 IF(ITYPE.EQ.1)THEN
  NPAR= DIOPLTGETNPAR(HISDATASET)
  PARS=>DIOPLTGETPARS(HISDATASET)
  LWP=.FALSE.; LWB=.FALSE.
  IF(TRIM(PARS(1)).EQ.'Waterlevel  (m AD)'.OR. &
     TRIM(PARS(1)).EQ.'Waterlevel mean (m AD)'.OR. &
     TRIM(PARS(1)).EQ.'Waterlevel max. (m AD)'.OR. &
     TRIM(PARS(1)).EQ.'Waterlevel mean (m A')LWP=.TRUE.
  IF(NPAR.GT.1)THEN
   IF(PARS(2).EQ.'Waterdepth  (m)'.OR. &
      PARS(2).EQ.'Waterdepth max. (m)'.OR. &
      PARS(2).EQ.'Waterdepth mean (m)')LWB=.TRUE.
  ENDIF
  IF(.NOT.LWP)THEN
   WRITE(IU(IOUT),'(A)') 'HIS-file does not contain the appropriate quantities:'
   WRITE(IU(IOUT),'(A)') '- Waterlevel  (m AD)'
   WRITE(IU(IOUT),'(A)') '  or'
   WRITE(IU(IOUT),'(A)') '- Waterlevel mean (m A)'
   IF(.NOT.LWB)WRITE(IU(IOUT),'(A)') '- Waterdepth  (m) <--- optinal'
   WRITE(IU(IOUT),'(A)') 'Available parameters are:'
   DO I=1,SIZE(PARS); WRITE(IU(IOUT),'(I5,A)') I,TRIM(PARS(I)); ENDDO
   IF(IBATCH.EQ.1)STOP 'error occured check tmp\imod2sobek.log'
   RETURN
  ENDIF
  WATERLEVELINDICES=1
  IF(LWB)WATERLEVELINDICES(2)=2
 ELSEIF(ITYPE.EQ.2)THEN
  NPAR= DIOPLTGETNPAR(SHISDATASET)
  PARS=>DIOPLTGETPARS(SHISDATASET)
  WATERLEVELINDICES(1)=2
  WATERLEVELINDICES(2)=3
 ENDIF

 !DO I=1,NPAR
 ! IF(PARS(I).EQ.QUANTITYID(1,ITYPE))WATERLEVELINDICES(1)=I
 ! IF(PARS(I).EQ.QUANTITYID(2,ITYPE))WATERLEVELINDICES(2)=I
 !ENDDO

 !WRITE(*,*) ITYPE,WATERLEVELINDICES

 IF(ITYPE.EQ.1)THEN
  NLOC= DIOPLTGETNLOC(HISDATASET)
  LOCS=>DIOPLTGETLOCS(HISDATASET)
 ELSEIF(ITYPE.EQ.2)THEN
  NLOC= DIOPLTGETNLOC(SHISDATASET)
  LOCS=>DIOPLTGETLOCS(SHISDATASET)
 ENDIF

 SELECTEDLOCATIONINDICES=0
 DO I=1,NLOC
  IF(TRIM(UTL_CAP(LOCS(I),'U')).EQ.TRIM(UTL_CAP(CLCID,'U')))SELECTEDLOCATIONINDICES(1)=I
 ENDDO
 IF(SELECTEDLOCATIONINDICES(1).EQ.0)THEN
  IF(ITYPE.EQ.1)WRITE(IU(IOUT),'(A)') 'CALCULATION POINT '//TRIM(CLCID)//' NOT FOUND IN '//TRIM(FNAME(IHIS))
  IF(ITYPE.EQ.2)WRITE(IU(IOUT),'(A)') 'WEIR '//TRIM(CLCID)//' NOT FOUND IN '//TRIM(FNAME(SHIS))
  NTIMES=0
  RETURN
 ENDIF

 !## fill time indices with 1-NTimes
 IF(ITYPE.EQ.1)NTIMES=DIOPLTGETNTIMES(HISDATASET)
 IF(ITYPE.EQ.2)NTIMES=DIOPLTGETNTIMES(SHISDATASET)

 IF(NTIMES.GT.DIMTIMES)THEN
  IF(ALLOCATED(SELECTEDVALUES))DEALLOCATE(SELECTEDVALUES)
  IF(ALLOCATED(JULIANTIMES))DEALLOCATE(JULIANTIMES)
  IF(ALLOCATED(TIMINDICES))DEALLOCATE(TIMINDICES)
  ALLOCATE(SELECTEDVALUES(2,1,NTIMES))
  ALLOCATE(TIMINDICES(NTIMES))
  ALLOCATE(JULIANTIMES(NTIMES))
  DIMTIMES=NTIMES
 ENDIF

 DO I=1,NTIMES
  TIMINDICES(I)=I
 ENDDO

 IF(ITYPE.EQ.1)JULIANTIMES=DIOPLTGETTIMES(HISDATASET)
 IF(ITYPE.EQ.2)JULIANTIMES=DIOPLTGETTIMES(SHISDATASET)

 !## Allocate result array (#parameters * 2 locations * #timestep), and get selection
 IF(ITYPE.EQ.1)THEN
  IF(DIOPLTGETSELECTION(HISDATASET,2,WATERLEVELINDICES,  &
                        1,SELECTEDLOCATIONINDICES,      &
                        NTIMES,TIMINDICES,SELECTEDVALUES))THEN
   IF(WATERLEVELINDICES(1).EQ.WATERLEVELINDICES(2))THEN
    !## create bottom-level from waterlevel
    SELECTEDVALUES(2,1,:)=SELECTEDVALUES(1,1,:) !-SELECTEDVALUES(2,1,:)
   ELSE
    !## create bottom-level from waterlevel and waterdepth
    SELECTEDVALUES(2,1,:)=SELECTEDVALUES(1,1,:)-SELECTEDVALUES(2,1,:)
   ENDIF
  ELSE
  ENDIF
 ELSEIF(ITYPE.EQ.2)THEN
  IF(DIOPLTGETSELECTION(SHISDATASET,2,WATERLEVELINDICES,  &
                        1,SELECTEDLOCATIONINDICES,      &
                        NTIMES,TIMINDICES,SELECTEDVALUES))THEN
  ELSE
  ENDIF

 ENDIF

 CALL ISDPROCESSDATA()

 END SUBROUTINE HIS1GETDATA

 !##=====================================================================
 SUBROUTINE ISDPROCESSDATA()
 !##=====================================================================
 IMPLICIT NONE
 INTEGER :: I,II,J,K

 !## compute mean waterlevel/bottomlevel
 J =INT(JULIANTIMES(1))
 K =0
 II=1
 DO I=1,NTIMES
  IF(INT(JULIANTIMES(I)).EQ.J)THEN
   K=K+1
   IF(K.GT.1)SELECTEDVALUES(:,1,II)=SELECTEDVALUES(:,1,II)+SELECTEDVALUES(:,1,I)
  ELSE
   IF(K.GT.0)SELECTEDVALUES(:,1,II)=SELECTEDVALUES(:,1,II)/REAL(K)
   JULIANTIMES(II)       =J+1
   J                     =INT(JULIANTIMES(I))
   II                    =II+1
   K                     =0
  ENDIF
 END DO

 IF(K.GT.0)SELECTEDVALUES(:,1,II)=SELECTEDVALUES(:,1,II)/REAL(K)
 JULIANTIMES(II)=J+1

 NTIMES=II

 END SUBROUTINE ISDPROCESSDATA

 !##=====================================================================
 SUBROUTINE ISC1CREATE(ICRS,IB,NCRS,IREFSC)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IB
 INTEGER,INTENT(INOUT) :: ICRS,IREFSC
 INTEGER,INTENT(OUT) :: NCRS
 INTEGER :: I,J,K,IC,II
 CHARACTER(LEN=MAXLEN) :: CROSNAME
 REAL :: KM,MIND

 !ICRS  =ICRS+1
 !IREFSC=IREFSC+1
 !WRITE(IU(ISC1),REC=ICRS+1) 3,IREFSC,REAL(TP2(IB)%AL)/2.0,'Profile'
 !WRITE(IU(ISC2),REC=IREFSC+1) -5.0,5.0,25.0
 !IREFSC=IREFSC+1
 !WRITE(IU(ISC2),REC=IREFSC+1)  0.0,0.0,25.0
 !IREFSC=IREFSC+1
 !WRITE(IU(ISC2),REC=IREFSC+1)  5.0,5.0,25.0
 !NCRS=1
 !RETURN

 !## initialize kM
 KM=25.0
 
 DO I=1,NBDFR
  IF(TRIM(TP2(IB)%ID).EQ.TRIM(BDFR(I)%CI))THEN
   KM=BDFR(I)%KM
   EXIT
  ENDIF
 END DO

 NCRS=0
 DO I=1,NCROS
  IF(TRIM(TP2(IB)%ID).EQ.TRIM(CR(I)%CI))THEN
   DO J=1,NDAT
    IF(TRIM(CR(I)%ID).EQ.TRIM(PDAT(J)%ID))THEN
     DO K=1,NDEF
      IF(TRIM(PDAT(J)%DI).EQ.TRIM(PDEF(K)%ID))THEN

       NCRS    =NCRS+1
       CROSNAME=PDEF(K)%ID

       ICRS    =ICRS+1
       WRITE(IU(ISC1),REC=ICRS+1) PDEF(K)%NNXY,IREFSC+1,REAL(CR(I)%LC),CROSNAME

       !## minimal value = zero!
       MIND         =MINVAL(PDEF(K)%YPROF(1:PDEF(K)%NNXY))
       PDEF(K)%YPROF=PDEF(K)%YPROF-MIND
       MIND         =0.0
       II           =0
       DO IC=1,PDEF(K)%NNXY
        IF(PDEF(K)%YPROF(IC).EQ.0.0)THEN
         MIND=MIND+PDEF(K)%XPROF(IC)
         II  =II+1
        ENDIF
       ENDDO
       MIND=MIND/REAL(II)
       PDEF(K)%XPROF=PDEF(K)%XPROF-MIND

       DO IC=1,PDEF(K)%NNXY
        IREFSC=IREFSC+1
        WRITE(IU(ISC2),REC=IREFSC+1) PDEF(K)%XPROF(IC),PDEF(K)%YPROF(IC),KM
       END DO

      ENDIF
     ENDDO
    ENDIF
   ENDDO
  ENDIF
 ENDDO

 IF(NCRS.EQ.0)WRITE(IU(IOUT),'(A)') '>>> No profile found for '//TRIM(TP2(IB)%ID)//' <<<'

 END SUBROUTINE ISC1CREATE

 !##=====================================================================
 SUBROUTINE IST1CREATE(ISTW,IB,NSTW,IREFST)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IB
 INTEGER,INTENT(INOUT) :: ISTW,IREFST
 INTEGER,INTENT(OUT) :: NSTW
 CHARACTER(LEN=MAXLEN) :: STUWNAME
 INTEGER :: I,J,N,ID

 !ISTW  =ISTW+1
 !IREFST=IREFST+1
 !WRITE(IU(IST1),REC=ISTW+1) 3,IREFST,REAL(TP2(IB)%AL)/2.0,'Stuw'
 !WRITE(IU(IST2),REC=IREFST+1) 20040101,20.0,15.0
 !IREFST=IREFST+1
 !WRITE(IU(IST2),REC=IREFST+1) 20040110,19.0,13.0
 !IREFST=IREFST+1
 !WRITE(IU(IST2),REC=IREFST+1) 20040131,17.0,11.0
 !NSTW=1

 NSTW=0
 DO I=1,NSTUW
  IF(TRIM(TP2(IB)%ID).EQ.TRIM(ST(I)%CI))THEN

 !#from his-file
   CALL HIS1GETDATA(ST(I)%ID,2)

   IF(NTIMES.GT.0)THEN

    NSTW    =NSTW+1
    ISTW    =ISTW+1
    N       =NTIMES
    STUWNAME=ST(I)%ID

    WRITE(IU(IST1),REC=ISTW+1) N,IREFST+1,REAL(ST(I)%LC),STUWNAME

    DO J=1,N
     IREFST=IREFST+1
     ID    =UTL_JDATETOIDATE(INT(JULIANTIMES(J)))
     WRITE(IU(IST2),REC=IREFST+1) ID,SELECTEDVALUES(1,1,J),SELECTEDVALUES(2,1,J)
    END DO
   ENDIF

  ENDIF
 ENDDO

 RETURN
 END SUBROUTINE

 !##=====================================================================
 SUBROUTINE ISQ1CREATE(IQHR,IB,NQHR,IREFSQ)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IB
 INTEGER,INTENT(INOUT) :: IQHR,IREFSQ
 INTEGER,INTENT(OUT) :: NQHR
 CHARACTER(LEN=MAXLEN) :: CNAME
 INTEGER :: I,N

 !IQHR  =IQHR+1
 !IREFSQ=IREFSQ+1                                          !123456789012
 !CNAME ='Qh '//TRIM(ITOS(IB))
 !WRITE(IU(ISQ1),REC=IQHR+1) 3,IREFSQ,REAL(TP2(IB)%AL)/2.0,CNAME
 !WRITE(IU(ISQ2),REC=IREFSQ+1) 20.0,15.0,10,6.0
 !IREFSQ=IREFSQ+1
 !WRITE(IU(ISQ2),REC=IREFSQ+1) 19.0,13.0,8.5,4.5
 !IREFSQ=IREFSQ+1
 !WRITE(IU(ISQ2),REC=IREFSQ+1) 17.0,11.0,6.0,2.3
 !NQHR=1

 NQHR=0
 DO I=1,NQHREL
  IF(TRIM(TP2(IB)%ID).EQ.TRIM(QH(I)%CI))THEN

   NQHR =NQHR+1
   IQHR =IQHR+1
   N    =2
   CNAME=QH(I)%ID

   IREFSQ=IREFSQ+1
   WRITE(IU(ISQ1),REC=IQHR+1) N,IREFSQ,REAL(QH(I)%LC),CNAME

   WRITE(IU(ISQ2),REC=IREFSQ+1) 20.0,11.0,10.0,2.3
   IREFSQ=IREFSQ+1
   WRITE(IU(ISQ2),REC=IREFSQ+1) 50.0,15.0,50.0,6.0

  ENDIF
 ENDDO

 END SUBROUTINE ISQ1CREATE

 !###======================================================================
 SUBROUTINE MAIN1ALNETWORKTP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,I

 NNODE=0; NNDLK=0; NBRCH=0
 READ(IU(ITP),*)
 DO
  READ(IU(ITP),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'NODE')THEN
   NNODE=NNODE+1
   I =INDEX(LINE,' id ')
   IF(I.LE.0)CALL MAINRDERROR('id',LINE)
  ELSEIF(INDEX(LINE,'NDLK').GT.0)THEN
   NNDLK=NNDLK+1
   I =INDEX(LINE,' id ')
   IF(I.LE.0)CALL MAINRDERROR('id',LINE)
  ELSEIF(INDEX(LINE,'BRCH').GT.0)THEN
   NBRCH=NBRCH+1
  ENDIF
 ENDDO

 REWIND(IU(ITP))

 WRITE(IU(IOUT),'(8X,I10,A)') NNODE,' nodes from '//TRIM(FNAME(ITP))
 WRITE(IU(IOUT),'(8X,I10,A)') NBRCH,' branches from '//TRIM(FNAME(ITP))
 WRITE(IU(IOUT),'(8X,I10,A)') NNDLK,' flow linkage nodes from '//TRIM(FNAME(ITP))

 END SUBROUTINE MAIN1ALNETWORKTP

 !###======================================================================
 SUBROUTINE MAIN1ALNETWORKCP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS

 !read number of curve-point from NETWORK.CP
 NCPNT=0
 READ(IU(ICP),*)
 DO
  READ(IU(ICP),*,IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)  
  IF(LINE(1:4).EQ.'BRCH')THEN
   READ(IU(ICP),*,IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(INDEX(LINE,'TBLE').GT.0)THEN
    DO
     READ(IU(ICP),*,IOSTAT=IOS) LINE
     IF(IOS.NE.0)EXIT
     IF(INDEX(LINE,'tble').GT.0)EXIT
     NCPNT=NCPNT+1
    END DO
   ENDIF
  ENDIF
 ENDDO

 REWIND(IU(ICP))

 WRITE(IU(IOUT),'(8X,I10,A)') NCPNT,' curve-points from '//TRIM(FNAME(ICP))

 END SUBROUTINE MAIN1ALNETWORKCP

 !###======================================================================
 SUBROUTINE MAIN1ALNETWORKCR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS

 !read number of crosssection from NETWORK.CR
 NCROS=0
 READ(IU(ICR),*)
 DO
  READ(IU(ICR),*,IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'CRSN')NCROS=NCROS+1
 ENDDO

 REWIND(IU(ICR))

 WRITE(IU(IOUT),'(8X,I10,A)') NCROS,' cross-sections from '//TRIM(FNAME(ICR))

 END SUBROUTINE MAIN1ALNETWORKCR

 !###======================================================================
 SUBROUTINE MAIN1ALNETWORKGR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS

 !read number of crosssection from NETWORK.CR
 NCALCP=0
 READ(IU(IGR),*)
 DO
  READ(IU(IGR),*,IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'GRID')THEN
   READ(IU(IGR),*,IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(INDEX(LINE,'GridPoint Table').GT.0)THEN
    READ(IU(IGR),*,IOSTAT=IOS) LINE
    IF(INDEX(LINE,'TBLE').GT.0)THEN
     DO
      READ(IU(IGR),*,IOSTAT=IOS) LINE
      IF(IOS.NE.0)EXIT
      IF(INDEX(LINE,'tble').GT.0)EXIT
      NCALCP=NCALCP+1
     END DO
    ENDIF
   ENDIF
  ENDIF
 ENDDO

 REWIND(IU(IGR))

 WRITE(IU(IOUT),'(8X,I10,A)') NCALCP,' calculation points from '//TRIM(FNAME(IGR))

 END SUBROUTINE MAIN1ALNETWORKGR

 !###======================================================================
 SUBROUTINE MAIN1ALNETWORKST()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS

 !## read number of crosssection from NETWORK.CR
 NSTUW=0
 READ(IU(IST),*)
 DO
  READ(IU(IST),*,IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'STRU')NSTUW=NSTUW+1
 ENDDO

 REWIND(IU(IST))

 WRITE(IU(IOUT),'(8X,I10,A)') NSTUW,' weirs points from '//TRIM(FNAME(IST))

 END SUBROUTINE MAIN1ALNETWORKST

 !###======================================================================
 SUBROUTINE MAIN1ALPROFILEDAT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS

 !get profile id's from PROFILE.DAT
 DO
  READ(IU(IAT),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'CRSN')NDAT=NDAT+1
 ENDDO

 REWIND(IU(IAT))

 WRITE(IU(IOUT),'(8X,I10,A)') NDAT,' profile dat from '//TRIM(FNAME(IAT))

 END SUBROUTINE MAIN1ALPROFILEDAT

 !###======================================================================
 SUBROUTINE MAIN1ALPROFILEDEF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS

 !## get profile id's from PROFILE.DEF
 NDEF=0
 DO
  READ(IU(IEF),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'CRDS')NDEF=NDEF+1
 ENDDO

 REWIND(IU(IEF))

 WRITE(IU(IOUT),'(8X,I10,A)') NDEF,' profile definitions from '//TRIM(FNAME(IEF))

 END SUBROUTINE MAIN1ALPROFILEDEF

 !###======================================================================
 SUBROUTINE MAIN1ALFRICTIONDAT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS

 !## get friction id's from FRICTION.DAT
 NBDFR=0
 DO
  READ(IU(IFR),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'BDFR')NBDFR=NBDFR+1
 ENDDO

 REWIND(IU(IFR))

 WRITE(IU(IOUT),'(8X,I10,A)') NBDFR,' bedfriction definitions from '//TRIM(FNAME(IFR))

 END SUBROUTINE MAIN1ALFRICTIONDAT

 !###======================================================================
 SUBROUTINE MAIN1RDNETWORKTP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: NN,NB,NL,I,IOS

 NN=0
 NB=0
 NL=0
 READ(IU(ITP),*)
 DO
  READ(IU(ITP),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'NODE')THEN

   NN=NN+1
   I=INDEX(LINE,' id '); IF(I.LE.0)CALL MAINRDERROR('id',LINE); READ(LINE(I+4:),*) TP1(NN)%ID
   I=INDEX(LINE,' px '); IF(I.LE.0)CALL MAINRDERROR('px',LINE); READ(LINE(I+4:),*) TP1(NN)%PX
   I=INDEX(LINE,' py '); IF(I.LE.0)CALL MAINRDERROR('py',LINE); READ(LINE(I+4:),*) TP1(NN)%PY

  ELSEIF(LINE(1:4).EQ.'NDLK')THEN

   NL=NL+1
   I=INDEX(LINE,' id '); IF(I.LE.0)CALL MAINRDERROR('id',LINE); READ(LINE(I+4:),*) TP3(NL)%ID
   I=INDEX(LINE,' px '); IF(I.LE.0)CALL MAINRDERROR('px',LINE); READ(LINE(I+4:),*) TP3(NL)%PX
   I=INDEX(LINE,' py '); IF(I.LE.0)CALL MAINRDERROR('py',LINE); READ(LINE(I+4:),*) TP3(NL)%PY
   I=INDEX(LINE,' ci '); IF(I.LE.0)CALL MAINRDERROR('ci',LINE); READ(LINE(I+4:),*) TP3(NL)%CI
   I=INDEX(LINE,' lc '); IF(I.LE.0)CALL MAINRDERROR('lc',LINE); READ(LINE(I+4:),*) TP3(NL)%LC

  ELSEIF(LINE(1:4).EQ.'BRCH')THEN

   NB=NB+1

   I=INDEX(LINE,' id '); IF(I.LE.0)CALL MAINRDERROR('id',LINE); READ(LINE(I+4:),*) TP2(NB)%ID
   I=INDEX(LINE,' bn '); IF(I.LE.0)CALL MAINRDERROR('bn',LINE); READ(LINE(I+4:),*) TP2(NB)%CBN
   I=INDEX(LINE,' en '); IF(I.LE.0)CALL MAINRDERROR('en',LINE); READ(LINE(I+4:),*) TP2(NB)%CEN
   I=INDEX(LINE,' al '); IF(I.LE.0)CALL MAINRDERROR('al',LINE); READ(LINE(I+4:),*) TP2(NB)%AL

  ENDIF
 ENDDO

 WRITE(IU(IOUT),'(8X,I10,A)') NN,' nodes from '//TRIM(FNAME(ITP))
 WRITE(IU(IOUT),'(8X,I10,A)') NB,' branches from '//TRIM(FNAME(ITP))
 WRITE(IU(IOUT),'(8X,I10,A)') NL,' flow connection nodes from '//TRIM(FNAME(ITP))

 END SUBROUTINE MAIN1RDNETWORKTP

 !###======================================================================
 SUBROUTINE MAIN1RDNETWORKCP()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=50) :: CPID
 INTEGER :: NC,I,J,IOS
 REAL :: D

 !## read number of curve-point from NETWORK.CP
 NC=0
 READ(IU(ICP),*)
 DO
  READ(IU(ICP),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  IF(INDEX(LINE,'BRCH').GT.0)THEN

   I=INDEX(LINE,' id '); IF(I.LE.0)CALL MAINRDERROR('id',LINE); READ(LINE(I+4:),*) CPID

   READ(IU(ICP),'(A256)',IOSTAT=IOS) LINE 
   IF(IOS.NE.0)EXIT
   LINE=ADJUSTL(LINE)
   IF(LINE(1:4).EQ.'TBLE')THEN
    J=0
    DO
     READ(IU(ICP),'(A256)',IOSTAT=IOS) LINE
     IF(IOS.NE.0)EXIT
     IF(INDEX(LINE,'tble').GT.0)EXIT
     NC       =NC+1
     CP(NC)%ID=TRIM(CPID)
     READ(LINE,*) CP(NC)%SLEN,CP(NC)%ANGLE

     !## correct slen for end of branch
     IF(J.GT.0)THEN
      D          =CP(NC)%SLEN-CP(NC-1)%SLEN
      CP(NC)%SLEN=(D*2.0)+CP(NC-1)%SLEN
     ELSE
      CP(NC)%SLEN=CP(NC)%SLEN*2.0
     ENDIF

     J=J+1

    END DO

   ENDIF
  ENDIF
 ENDDO

 WRITE(IU(IOUT),'(8X,I10,A)') NC,' curve points from '//TRIM(FNAME(ICP))

 END SUBROUTINE MAIN1RDNETWORKCP

 !###======================================================================
 SUBROUTINE MAIN1RDNETWORKCR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,NC,I

 NC=0
 READ(IU(ICR),*)

 !## get profile location from NETWORK.CR
 DO
  READ(IU(ICR),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'CRSN')THEN

   NC=NC+1

   I=INDEX(LINE,' lc '); IF(I.LE.0)CALL MAINRDERROR('lc',LINE); READ(LINE(I+4:),*) CR(NC)%LC
   I=INDEX(LINE,' ci '); IF(I.LE.0)CALL MAINRDERROR('ci',LINE); READ(LINE(I+4:),*) CR(NC)%CI
   I=INDEX(LINE,' id '); IF(I.LE.0)CALL MAINRDERROR('id',LINE); READ(LINE(I+4:),*) CR(NC)%ID

  ENDIF
 ENDDO

 WRITE(IU(IOUT),'(8X,I10,A)') NC,' cross-sections from '//TRIM(FNAME(ICR))

 END SUBROUTINE MAIN1RDNETWORKCR

 !###======================================================================
 SUBROUTINE MAIN1RDNETWORKGR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,NC,NQ,I,J,IB
 CHARACTER(LEN=50) :: CI,DUMMY1,DUMMY2,DUMMY3,DUMMY4

 !## read number of crosssection from NETWORK.GR
 NC=0
 NQ=0
 READ(IU(IGR),*)
 DO
  READ(IU(IGR),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'GRID')THEN

   I=INDEX(LINE,' ci ')
   IF(I.LE.0)CALL MAINRDERROR('ci',LINE)

   READ(LINE(I+4:),*) CI

   READ(IU(IGR),'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT

   IF(INDEX(LINE,'GridPoint Table').GT.0)THEN

    READ(IU(IGR),*,IOSTAT=IOS) LINE
    LINE=ADJUSTL(LINE)
    IF(LINE(1:4).EQ.'TBLE')THEN
     J=0
     DO
      READ(IU(IGR),'(A256)',IOSTAT=IOS) LINE
      IF(IOS.NE.0)EXIT
      IF(INDEX(LINE,'tble').GT.0)EXIT

      J =J+1
      NC=NC+1

      GR(NC)%CI=TRIM(CI)
      READ(LINE,*) GR(NC)%LC,DUMMY1,DUMMY2,GR(NC)%ID,DUMMY3

      !## include qh-points
      IF(J.GT.1)THEN
       NQ       = NQ+1
       QH(NQ)%CI= TRIM(CI)
       QH(NQ)%LC= GR(NC-1)%LC+((GR(NC)%LC-GR(NC-1)%LC)/2.0)
       QH(NQ)%ID= DUMMY4
      ENDIF
      DUMMY4=DUMMY3

     END DO
    ENDIF

    DO IB=1,NBRCH
     !## adjust maximum length to actual length
     IF(TRIM(TP2(IB)%ID).EQ.TRIM(GR(NC)%CI))THEN
      GR(NC)%LC=TP2(IB)%AL
      QH(NQ)%LC=GR(NC-1)%LC+((GR(NC)%LC-GR(NC-1)%LC)/2.0)
     ENDIF
    ENDDO

   ENDIF
  ENDIF
 ENDDO

 NQHREL=NQ
 WRITE(IU(IOUT),'(8X,I10,A)') NC,' calculation points from '//TRIM(FNAME(IGR))
 WRITE(IU(IOUT),'(8X,I10,A)') NQ,' qh points from '//TRIM(FNAME(IGR))

 END SUBROUTINE MAIN1RDNETWORKGR

 !###======================================================================
 SUBROUTINE MAIN1RDNETWORKST()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,NS,I

 !## read number of crosssection from NETWORK.CR
 NS=0
 READ(IU(IST),*)
 DO
  READ(IU(IST),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'STRU')THEN
   NS=NS+1

   I=INDEX(LINE,' lc '); IF(I.LE.0)CALL MAINRDERROR('lc',LINE); READ(LINE(I+4:),*) ST(NS)%LC
   I=INDEX(LINE,' id '); IF(I.LE.0)CALL MAINRDERROR('id',LINE); READ(LINE(I+4:),*) ST(NS)%ID
   I=INDEX(LINE,' ci '); IF(I.LE.0)CALL MAINRDERROR('ci',LINE); READ(LINE(I+4:),*) ST(NS)%CI

  ENDIF
 ENDDO

 WRITE(IU(IOUT),'(8X,I10,A)') NS,' weirs points from '//TRIM(FNAME(IST))

 END SUBROUTINE MAIN1RDNETWORKST

 !###======================================================================
 SUBROUTINE MAIN1RDPROFILEDAT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,ND,I,J,K

 !## get profile id's from PROFILE.DAT
 ND=0
 DO
  READ(IU(IAT),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'CRSN')THEN

   I=INDEX(LINE,' id ')
   J=INDEX(LINE,' di ')
   K=INDEX(LINE,' rl ')

   IF(I.GT.0.AND.J.GT.0)THEN
    ND=ND+1
    !## get cross-section id
    READ(LINE(I+4:),*) PDAT(ND)%ID
    !## get cross-section definition-id
    READ(LINE(J+4:),*) PDAT(ND)%DI
    !## get cross-section definition-id
    IF(K.GT.0)READ(LINE(K+4:),*) PDAT(ND)%RL
   ENDIF

  ENDIF
 ENDDO

 WRITE(IU(IOUT),'(8X,I10,A)') ND,' cross-sections from '//TRIM(FNAME(IAT))

 END SUBROUTINE MAIN1RDPROFILEDAT

 !###======================================================================
 SUBROUTINE MAIN1RDPROFILEDEF()
 !###======================================================================
 IMPLICIT NONE
 REAL :: X,BL,BW,BS,OR,DOR,RD
 INTEGER :: IOS,ND,I,J,N
 REAL,POINTER,DIMENSION(:) :: XPROF,YPROF,XPROF_DUM,YPROF_DUM
 CHARACTER(LEN=256) :: LINE
 
 !## get profile definitions from PROFILES.DEF
 ND=0
 DO
  READ(IU(IEF),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT; LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'CRDS')THEN

   ND=ND+1

   I=INDEX(LINE,' id '); IF(I.LE.0)CALL MAINRDERROR('id',LINE); READ(LINE(I+4:),*) PDEF(ND)%ID
   I=INDEX(LINE,' ty '); IF(I.LE.0)CALL MAINRDERROR('ty',LINE); READ(LINE(I+4:),*) PDEF(ND)%PTYPE

   SELECT CASE (PDEF(ND)%PTYPE)

    !## trapeziodal
    CASE (1)
     I=INDEX(LINE,' bl ')
     IF(I.LE.0)THEN
      BL=0.0
     ELSE
      READ(LINE(I+4:),*) BL
     ENDIF
     I=INDEX(LINE,' bw ')
     IF(I.LE.0)CALL MAINRDERROR('bw',LINE)
     READ(LINE(I+4:),*) BW
     I=INDEX(LINE,' bs ')
     IF(I.LE.0)CALL MAINRDERROR('bs',LINE)
     READ(LINE(I+4:),*) BS

     PDEF(ND)%NNXY    =4

     ALLOCATE(PDEF(ND)%XPROF(PDEF(ND)%NNXY),PDEF(ND)%YPROF(PDEF(ND)%NNXY))

     PDEF(ND)%XPROF(2)=-BW/2.0     
     PDEF(ND)%XPROF(1)= PDEF(ND)%XPROF(2)-5.0
     PDEF(ND)%XPROF(3)= BW/2.0
     PDEF(ND)%XPROF(4)= PDEF(ND)%XPROF(3)+5.0
     PDEF(ND)%YPROF(1)= BL+ABS(PDEF(ND)%XPROF(1))*BS
     PDEF(ND)%YPROF(2)= BL
     PDEF(ND)%YPROF(3)= BL
     PDEF(ND)%YPROF(4)= BL+PDEF(ND)%XPROF(4)*BS

    !## tabulated(0) --- heigth and total width for that heigth
    CASE (0)

     DO
      READ(IU(IEF),'(A256)',IOSTAT=IOS) LINE
      IF(IOS.NE.0)EXIT; LINE=ADJUSTL(LINE); IF(LINE(1:4).EQ.'TBLE')EXIT
     ENDDO
     IF(IOS.EQ.0)THEN

      PDEF(ND)%NNXY=0; ALLOCATE(XPROF(MAXPROF),YPROF(MAXPROF))

      DO
       READ(IU(IEF),'(A256)',IOSTAT=IOS) LINE
       IF(IOS.NE.0)EXIT; IF(INDEX(LINE,'tble').GT.0)EXIT
       PDEF(ND)%NNXY=PDEF(ND)%NNXY+1
       N=PDEF(ND)%NNXY
       IF(N.GT.SIZE(XPROF))THEN
        ALLOCATE(XPROF_DUM(N*2),YPROF_DUM(N*2))
        XPROF_DUM(1:N-1)=XPROF(1:N-1)
        YPROF_DUM(1:N-1)=YPROF(1:N-1)
        DEALLOCATE(XPROF,YPROF)
        XPROF=>XPROF_DUM; YPROF=>YPROF_DUM     
       ENDIF
       !## read heigth and flowing width
       READ(LINE,*) YPROF(PDEF(ND)%NNXY),X,XPROF(PDEF(ND)%NNXY)
      ENDDO

      N=PDEF(ND)%NNXY
      ALLOCATE(PDEF(ND)%XPROF(N*2),PDEF(ND)%YPROF(N*2))
      PDEF(ND)%XPROF(1:N)=XPROF(1:N)
      PDEF(ND)%YPROF(1:N)=YPROF(1:N)

      !## devide by 2
      PDEF(ND)%XPROF=PDEF(ND)%XPROF/2.0
!      DO I=2,PDEF(ND)%NNXY
!       PDEF(ND)%XPROF(I)=PDEF(ND)%XPROF(I-1)+PDEF(ND)%XPROF(I)
!      END DO

      J=PDEF(ND)%NNXY
      DO I=1,PDEF(ND)%NNXY
       J                = J+1
       PDEF(ND)%XPROF(J)=-PDEF(ND)%XPROF(I)
       PDEF(ND)%YPROF(J)= PDEF(ND)%YPROF(I)
      END DO
      PDEF(ND)%NNXY=J
      CALL UTL_QKSORT2(PDEF(ND)%XPROF,PDEF(ND)%YPROF,PDEF(ND)%NNXY,PDEF(ND)%NNXY)
     
     ENDIF
     
    !## closed circle
    CASE (4)
    
     !## bedlevel
     I=INDEX(LINE,' bl ')
     IF(I.LE.0)THEN
      BL=0.0
     ELSE
      READ(LINE(I+4:),*) BL
     ENDIF

     I=INDEX(LINE,' rd ')
     IF(I.LE.0)CALL MAINRDERROR('rd',LINE)
     READ(LINE(I+4:),*) RD

     N=11; PDEF(ND)%NNXY=N

     ALLOCATE(PDEF(ND)%XPROF(PDEF(ND)%NNXY),PDEF(ND)%YPROF(PDEF(ND)%NNXY))
     OR =0.5*(2.0*PI)
     DOR=0.05*(2.0*PI)
     OR=OR+DOR
     DO I=1,N
      OR=OR-DOR
      PDEF(ND)%XPROF(I)=0.0+RD*COS(OR)
      PDEF(ND)%YPROF(I)= RD-RD*SIN(OR)
     ENDDO
     
    !## xz(10,11)
    CASE (10,11)

     I=INDEX(LINE,' ty ')
     !## skip tabulated storage widths
     IF(I.LE.0)THEN
      DO; READ(IU(IEF),'(A256)',IOSTAT=IOS) LINE
       IF(IOS.NE.0)EXIT; LINE=ADJUSTL(LINE); IF(LINE(1:4).EQ.'tble')EXIT
      ENDDO
     ENDIF

     DO
      READ(IU(IEF),'(A256)',IOSTAT=IOS) LINE
      IF(IOS.NE.0)EXIT; LINE=ADJUSTL(LINE); IF(LINE(1:4).EQ.'TBLE')EXIT
     ENDDO
     IF(IOS.EQ.0)THEN

      PDEF(ND)%NNXY=0; ALLOCATE(XPROF(MAXPROF),YPROF(MAXPROF))

      DO
       READ(IU(IEF),'(A256)',IOSTAT=IOS) LINE
       IF(IOS.NE.0)EXIT; IF(INDEX(LINE,'tble').GT.0)EXIT
       PDEF(ND)%NNXY=PDEF(ND)%NNXY+1
       N=PDEF(ND)%NNXY
       IF(N.GT.SIZE(XPROF))THEN
        ALLOCATE(XPROF_DUM(N*2),YPROF_DUM(N*2))
        XPROF_DUM(1:N-1)=XPROF(1:N-1)
        YPROF_DUM(1:N-1)=YPROF(1:N-1)
        DEALLOCATE(XPROF,YPROF)
        XPROF=>XPROF_DUM; YPROF=>YPROF_DUM     
       ENDIF
       !## reading horizontal and vertical distances
       READ(LINE,*) XPROF(PDEF(ND)%NNXY),YPROF(PDEF(ND)%NNXY)
      ENDDO

      N=PDEF(ND)%NNXY; ALLOCATE(PDEF(ND)%XPROF(N),PDEF(ND)%YPROF(N))
      PDEF(ND)%XPROF(1:N)=XPROF(1:N)
      PDEF(ND)%YPROF(1:N)=YPROF(1:N)

     ENDIF

    CASE DEFAULT
     LINE='Do not recognize profile type '//TRIM(ITOS(PDEF(ND)%PTYPE))
     WRITE(IU(IOUT),'(A)') TRIM(LINE)
     PDEF(ND)%NNXY=0

   END SELECT

  ENDIF
 ENDDO

 DEALLOCATE(XPROF,YPROF)

 WRITE(IU(IOUT),'(8X,I10,A)') ND,' cross-section definitions from '//TRIM(FNAME(IEF))
 
 END SUBROUTINE MAIN1RDPROFILEDEF

 !###======================================================================
 SUBROUTINE MAIN1RDFRICTIONDAT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IOS,NB,I,J

 !## get friction id's from FRICTION.DAT
 NB=0
 DO
  READ(IU(IFR),'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  LINE=ADJUSTL(LINE)
  IF(LINE(1:4).EQ.'BDFR')THEN
   I=INDEX(LINE,' id ')
   J=INDEX(LINE,' ci ')

   IF(I.GT.0.AND.J.GT.0)THEN
    NB=NB+1
    !## get cross-section id
    READ(LINE(I+4:),*) BDFR(NB)%ID
    !## get cross-section definition-id
    READ(LINE(J+4:),*) BDFR(NB)%CI
   ENDIF

 !#nog iets met kd doen!
   BDFR(NB)%KM=25.0

  ENDIF
 ENDDO

 !BDFR id '64' ci '64' mf 3 mt cp 0 25 0 mr cp 0 25 0 s1 6 s2 6     bdfr

 WRITE(IU(IOUT),'(8X,I10,A)') NB,' bedfriction definitions from '//TRIM(FNAME(IFR))

 END SUBROUTINE MAIN1RDFRICTIONDAT

 !###======================================================================
 SUBROUTINE MAINRDERROR(CERROR,LINE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CERROR,LINE

 WRITE(IU(IOUT),'(/1X,A/)') 'Can not find keyword ['//TRIM(CERROR)//'] in line:'
 WRITE(IU(IOUT),'(1X,A)') TRIM(LINE)
 IF(IBATCH.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find keyword ['//TRIM(CERROR)//'] in line:'//CHAR(13)// &
   TRIM(LINE),'Error')
 ELSE
  WRITE(*,'(/1X,A/)') 'Can not find keyword ['//TRIM(CERROR)//'] in line:'
  WRITE(*,'(1X,A)') TRIM(LINE)
  STOP
 ENDIF
 
 END SUBROUTINE
 
END MODULE MOD_SOBEK
