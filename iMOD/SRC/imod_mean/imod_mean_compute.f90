!!  Copyright (C) Stichting Deltares, 2005-2014.
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
MODULE MOD_MEAN_CLC

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MPW
USE MOD_POLINT, ONLY : POL1LOCATE
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_MESSAGEHANDLE,UTL_WAITMESSAGE,IDATETOJDATE,JDATETOIDATE,UTL_DIRINFO,GDATE,NEWLINE, &
 UTL_IDFSNAPTOGRID, IDATETOGDATE,UTL_GETMED
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IDF, ONLY : IDFOPEN,IDFREADDIM,IDFWRITE,IDFALLOCATEX,IDFALLOCATESXY,IDFDEALLOCATEX,IDFNULLIFY,IDF_EXTENT, &
                    IDFREAD,IDFDEALLOCATE,IDFGETVAL,IDFCOPY,IDFGETLOC,IDFIROWICOL,IDFREADPART,IDFFILLCOMMENT,IDFREADSCALE_GETX
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_OSD, ONLY : OSD_OPEN,OSD_GETENV
USE MOD_TOOLS_UTL, ONLY : TOOLSFILLPOINTER
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE
USE MOD_MEAN_PAR

TYPE(IDFOBJ),PRIVATE,ALLOCATABLE,DIMENSION(:) :: IDF
TYPE(IDFOBJ),PRIVATE :: IDFCP !## pointer idf to be computed
TYPE(IDFOBJ),PRIVATE :: IDFRP !## pointer idf to be read
INTEGER,PRIVATE,ALLOCATABLE,DIMENSION(:) :: ILIST,JLIST
CHARACTER(LEN=256),PRIVATE,ALLOCATABLE,DIMENSION(:) :: LISTNAME

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION MEAN1COMPUTE()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=512) :: LINE,LINEP
 REAL :: XC,YC,XVAL,XMIN,YMAX,XMAX,YMIN
 INTEGER :: IRAT,IRAT1,I,J,II,NFILES,IIDF,K,L,JD1,JD2,IY,IM,ID,IROW,ICOL,IU,JROW,JCOL,IEQ,ILAY
 INTEGER :: FYR,FMN,FDY,TYR,TMN,TDY  
 INTEGER :: NIP
 LOGICAL :: LEX
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE
 CHARACTER(LEN=52) :: IDFFILE,WC
 REAL,ALLOCATABLE,DIMENSION(:) :: XPERC
 REAL,DIMENSION(1) :: XMED
  
 MEAN1COMPUTE =.FALSE.

 !## entire area
 IF(MEAN_ISEL.EQ.1)THEN
  SHPIACT=0
 !## select all polygons
 ELSEIF(MEAN_ISEL.EQ.2)THEN
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,0,MEAN_GENFNAME)
  SHPIACT(1:SHPNO)=1
 !## usage of idf
 ELSEIF(MEAN_ISEL.EQ.3)THEN
  IF(.NOT.IDFREAD(IDFRP,MEAN_IDFNAME,0))RETURN
 ENDIF

 CALL WINDOWSELECT(0)

 IDFFILE    =MEAN_RESDIR(INDEX(MEAN_RESDIR,'\',.TRUE.)+1:)
 WC         =IDFFILE
 MEAN_RESDIR=MEAN_RESDIR(:INDEX(MEAN_RESDIR,'\',.TRUE.)-1)

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(MEAN_RESDIR)//'\summary_imod_mean_'//TRIM(OSD_GETENV('USERNAME'))//'.txt',STATUS='UNKNOWN')

 CALL UTL_MESSAGEHANDLE(0)

 DO II=1,MAX(1,MEAN_NLAYER)

  IF(MEAN_NLAYER.EQ.0)THEN
   J=INDEX(IDFFILE,'\',.TRUE.)
   IF(INDEX(IDFFILE,'*',.TRUE.).GT.J)IDFFILE=IDFFILE(:INDEX(IDFFILE,'*',.TRUE.)-1)
   IF(INDEX(IDFFILE,'.',.TRUE.).GT.J)IDFFILE=IDFFILE(:INDEX(IDFFILE,'.',.TRUE.)-1)
   MEAN_FMEAN(II) =TRIM(MEAN_RESDIR)//'\'//TRIM(CFUNC)//'_'//TRIM(IDFFILE)//'.IDF'
   MEAN_FTOTAL(II)=TRIM(MEAN_RESDIR)//'\TOTAL_'//TRIM(CFUNC)//'_'//TRIM(IDFFILE)//'.IDF'
   ILAY=0
  ELSE
   ILAY=MEAN_ILAYER(II)
   
   !## divide full date into separate parts    
   CALL IDATETOGDATE(MEAN_FYR,FYR,FMN,FDY)
   CALL IDATETOGDATE(MEAN_TYR,TYR,TMN,TDY)
   
   !## Concatenate years to layer files output files!
   MEAN_FMEAN(II) =TRIM(MEAN_RESDIR)//'\'//TRIM(IDFFILE)//'_'//TRIM(CFUNC)//'_'// &                                 
                   TRIM(ITOS(FYR))//'-'//TRIM(ITOS(FMN))//'-'//TRIM(ITOS(FDY))//'_to_'// &
                   TRIM(ITOS(TYR))//'-'//TRIM(ITOS(TMN))//'-'//TRIM(ITOS(TDY))//'_L'//TRIM(ITOS(ILAY))//'.IDF'
   IF(TRIM(CFUNC).EQ.'MEAN')THEN
    MEAN_FTOTAL(II)=TRIM(MEAN_RESDIR)//'\'//TRIM(IDFFILE)//'_count_'// &                                           ! FR 20131011
                    TRIM(ITOS(FYR))//'-'//TRIM(ITOS(FMN))//'-'//TRIM(ITOS(FDY))//'_to_'// &
                    TRIM(ITOS(TYR))//'-'//TRIM(ITOS(TMN))//'-'//TRIM(ITOS(TDY))//'_L'//TRIM(ITOS(ILAY))//'.IDF'
   ELSE
    MEAN_FTOTAL(II)=TRIM(MEAN_RESDIR)//'\'//TRIM(IDFFILE)//'_date_'//TRIM(CFUNC)//'_'// &                          ! FR 20131011  
                    TRIM(ITOS(FYR))//'-'//TRIM(ITOS(FMN))//'-'//TRIM(ITOS(FDY))//'_to_'// &
                    TRIM(ITOS(TYR))//'-'//TRIM(ITOS(TMN))//'-'//TRIM(ITOS(TDY))//'_L'//TRIM(ITOS(ILAY))//'.IDF'
   ENDIF
  ENDIF
  
  INQUIRE(FILE=MEAN_FMEAN(II),EXIST=LEX)
  IF(LEX)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
     TRIM(MEAN_FMEAN(II))//CHAR(13)//'allready exists overwrite it and continue?','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF
  ENDIF
  INQUIRE(FILE=MEAN_FTOTAL(II),EXIST=LEX)
  IF(LEX)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
     TRIM(MEAN_FTOTAL(II))//CHAR(13)//'allready exists overwrite it and continue?','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF
  ENDIF

  CALL IOSDIRENTRYTYPE('F')
  IF(MEAN_NLAYER.EQ.0)THEN
   CALL IOSDIRCOUNT(TRIM(MEAN_RESDIR),TRIM(WC),NFILES)
  ELSE
   CALL IOSDIRCOUNT(TRIM(MEAN_RESDIR),TRIM(IDFFILE)//'*_L'//TRIM(ITOS(ILAY))//'.IDF',NFILES)
  ENDIF

  IF(NFILES.LE.0)THEN
   IF(MEAN_NLAYER.EQ.0)THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No '//MEAN_RESDIR(INDEX(MEAN_RESDIR,'\',.TRUE.)+1: )//' Files found for specified period', 'Error')
    IF(IBATCH.EQ.1)WRITE(*,*) 'No '//MEAN_RESDIR(INDEX(MEAN_RESDIR,'\',.TRUE.)+1:)//' Files found for specified period'
   ELSE
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No '//TRIM(IDFFILE)//' Files found for specified period', 'Error')
    IF(IBATCH.EQ.1)WRITE(*,*) 'No '//TRIM(IDFFILE)//' Files found for specified period'
   ENDIF
   RETURN
  ENDIF

  ALLOCATE(LISTNAME(NFILES),ILIST(NFILES),JLIST(NFILES))
  IF(MEAN_NLAYER.EQ.0)THEN
   CALL UTL_DIRINFO(TRIM(MEAN_RESDIR),TRIM(WC),LISTNAME,NFILES,'F')
  ELSE
   CALL UTL_DIRINFO(TRIM(MEAN_RESDIR),TRIM(IDFFILE)//'*_L'//TRIM(ITOS(ILAY))//'.IDF',LISTNAME,NFILES,'F')
  ENDIF
  
  ALLOCATE(IDF(-1:NFILES))
  !## nullify idf's
  DO I=-1,NFILES; CALL IDFNULLIFY(IDF(I)); ENDDO

  IF(MEAN_NLAYER.EQ.0)THEN
   JD1=0; JD2=0
  ELSE
   JD1=IDATETOJDATE(MEAN_FYR); JD2=IDATETOJDATE(MEAN_TYR)
  ENDIF
 
  K=0
  L=0
  DO I=1,NFILES
   IF(.NOT.IDFREAD(IDF(I),TRIM(MEAN_RESDIR)//'\'//LISTNAME(I),0))THEN
    CALL MEAN1ABORT()
    RETURN
   ENDIF
   IF(MEAN_NLAYER.EQ.0)THEN
    LEX=.TRUE.
   ELSE
    LEX=IDF(I)%JD.GE.JD1.AND.IDF(I)%JD.LE.JD2.AND.IDF(I)%ILAY.EQ.ILAY
   ENDIF
   IF(LEX.AND.MEAN_NYEAR.GT.0)THEN
    !## within outer time constraints
    CALL GDATE(IDF(I)%JD,IY,IM,ID)
    !## check year
    DO J=1,MEAN_NYEAR; IF(IY.EQ.MEAN_IYEAR(J))EXIT; ENDDO
    LEX=J.LE.MEAN_NYEAR
   ENDIF
   !## check period: if nperiod.gt.0
   IF(LEX.AND.MEAN_NPERIOD.GT.0)THEN
    DO J=1,MEAN_NPERIOD,2
     IF(IM*100+ID.GE.MEAN_IPERIOD(J  ,2)*100+MEAN_IPERIOD(J  ,1).AND. &
        IM*100+ID.LE.MEAN_IPERIOD(J+1,2)*100+MEAN_IPERIOD(J+1,1))EXIT
    END DO
    LEX=J.LE.MEAN_NPERIOD
   ENDIF
   !## add current file to list to be processed
   IF(LEX)THEN
    K       =K+1
    IF(MEAN_NLAYER.EQ.0)THEN; ILIST(K)=I
    ELSE; ILIST(K)=IDF(I)%JD; ENDIF
    JLIST(K)=I
   ELSE
    L=L+1
   ENDIF
  END DO

  IF(K.LE.0)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No correct '//TRIM(IDFFILE)//' Files found for specified period!','Error')
   ELSE
    WRITE(*,*) 'No correct '//TRIM(IDFFILE)//' Files found for specified period!'
   ENDIF
   CALL MEAN1ABORT()
   RETURN
  ENDIF
  IF(K.EQ.1)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Only ONE correct '//TRIM(IDFFILE)//' files found!','Error')
   ELSE
    WRITE(*,'(A)') 'Only ONE correct '//TRIM(IDFFILE)//' files found for specified period!'
   ENDIF
   CALL MEAN1ABORT()
   RETURN
  ENDIF

  !## number of files within time-interval
  NFILES=K

  !## sort julian dates
  CALL WSORT(ILIST,1,NFILES)

  WRITE(IU,'(A)') 'Writing :'//TRIM(MEAN_FMEAN(II))
  WRITE(IU,'(A)') 'Writing :'//TRIM(MEAN_FTOTAL(II))
  WRITE(IU,*) 'Following files (',NFILES,') were CORRECT and were included:'
  DO I=1,NFILES
   IIDF=JLIST(I)
   LINE=TRIM(IDF(IIDF)%FNAME) 
   WRITE(IU,'(I10,A)') I,' '//TRIM(LINE)
   IF(IBATCH.EQ.1)WRITE(*,'(1X,I10,A)') I,' '//TRIM(LINE)
  END DO
  CALL FLUSH(IU)

  CALL IDFCOPY(IDF(1),IDF(0))
  IF(.NOT.IDFALLOCATEX(IDF(0)))THEN; ENDIF
  CALL IDFCOPY(IDF(1),IDF(-1))
  IF(.NOT.IDFALLOCATEX(IDF(-1)))THEN; ENDIF
  
  IRAT =0; IRAT1=IRAT

  !## determine whether not all idf"s are the same
  LEX=.TRUE.
  DO I=1,NFILES
   IIDF=JLIST(I)
   IF(I.EQ.1)THEN
    XMIN=IDF(IIDF)%XMIN
    XMAX=IDF(IIDF)%XMAX
    YMIN=IDF(IIDF)%YMIN
    YMAX=IDF(IIDF)%YMAX
    IEQ =IDF(IIDF)%IEQ
   ELSE
    IF(XMIN.NE.IDF(IIDF)%XMIN.OR. &
       YMIN.NE.IDF(IIDF)%YMIN.OR. &
       XMIN.NE.IDF(IIDF)%XMIN.OR. &
       YMAX.NE.IDF(IIDF)%YMAX.OR. &
       IEQ .NE.IDF(IIDF)%IEQ)LEX=.FALSE.
   ENDIF
  END DO

  !## copy settings --- USE FIRST ONE
  CALL IDFCOPY(IDF(0),IDFCP)
  IF(.NOT.IDFALLOCATEX(IDFCP))THEN; ENDIF
  !## create pointer in ipidf - only once to be created
  CALL TOOLSFILLPOINTER(MEAN_ISEL,IDFRP,IDFCP,NIP)
  !## deallocate pointer idf read
  IF(MEAN_ISEL.EQ.3)CALL IDFDEALLOCATEX(IDFRP)

  IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF(0)%NROW,'Progress 0%')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Busy ...'

  SELECT CASE (TRIM(CFUNC))
   CASE ('MEAN','SUM')
    IDF(-1)%X= 0.0; IDF(0)%X=  0.0
   CASE ('MIN')
    IDF(-1)%X= 0.0; IDF(0)%X= 10.0E10
   CASE ('MAX')
    IDF(-1)%X= 0.0; IDF(0)%X=-10.0E10
   CASE ('PERC')
    ALLOCATE(XPERC(NFILES))
  END SELECT
  
  IF(LEX)THEN
   DO IROW=1,IDF(0)%NROW
    CALL WMESSAGEPEEK(ITYPE,MESSAGE)
    DO ICOL=1,IDF(0)%NCOL
     IF(IDFCP%X(ICOL,IROW).NE.IDFCP%NODATA)THEN

      DO I=1,NFILES
       IIDF=JLIST(I)
       !## get idfvalue
       XVAL=IDFGETVAL(IDF(IIDF),IROW,ICOL)
       IF(XVAL.NE.IDF(IIDF)%NODATA)THEN
        SELECT CASE (TRIM(CFUNC))
         CASE ('MEAN','SUM')
          IDF( 0)%X(ICOL,IROW)=IDF( 0)%X(ICOL,IROW)+XVAL
          IDF(-1)%X(ICOL,IROW)=IDF(-1)%X(ICOL,IROW)+1.0
         CASE ('MIN')
          IF(XVAL.LT.IDF(0)%X(ICOL,IROW))THEN
           IDF( 0)%X(ICOL,IROW)=XVAL
           IDF(-1)%X(ICOL,IROW)=IDF(IIDF)%JD
          ENDIF
         CASE ('MAX')
          IF(XVAL.GT.IDF(0)%X(ICOL,IROW))THEN
           IDF( 0)%X(ICOL,IROW)=XVAL
           IDF(-1)%X(ICOL,IROW)=IDF(IIDF)%JD
          ENDIF
        CASE ('PERC')
         IDF(-1)%X(ICOL,IROW)=IDF(-1)%X(ICOL,IROW)+1.0
         XPERC(I)            =XVAL
        END SELECT          
       ENDIF
      END DO
      IF((TRIM(CFUNC)).EQ.'PERC')THEN
       CALL UTL_GETMED(XPERC,INT(IDF(-1)%X(ICOL,IROW)),-9999.99,(/0.5/),1,I,XMED)
       IDF(0)%X(ICOL,IROW)=XMED(1)
      ENDIF
     ENDIF
    ENDDO
    IF(IBATCH.EQ.0)THEN
     CALL WINDOWSELECT(0)
     CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF(0)%NROW,'Progress Equal IDF"s: ')
    ELSE
     WRITE(6,'(A,F10.2,A)') '+Progress Equal IDF"s: ',REAL(IROW*100)/REAL(IDF(0)%NROW),'%'    ! FR 20131011
    ENDIF
   ENDDO
  ELSE
   DO IROW=1,IDF(0)%NROW
    CALL WMESSAGEPEEK(ITYPE,MESSAGE)
    DO ICOL=1,IDF(0)%NCOL
     IF(IDFCP%X(ICOL,IROW).NE.IDFCP%NODATA)THEN

      !## get x/y coordinate
      CALL IDFGETLOC(IDF(0),IROW,ICOL,XC,YC)

      DO I=1,NFILES
       IIDF=JLIST(I)
       !## get irow/icol
       CALL IDFIROWICOL(IDF(IIDF),JROW,JCOL,XC,YC)
       IF(JROW.GE.1.AND.JROW.LE.IDFCP%NROW.AND. &
          JCOL.GE.1.AND.JCOL.LE.IDFCP%NCOL)THEN
        !## get idfvalue
        XVAL=IDFGETVAL(IDF(IIDF),JROW,JCOL)
        IF(XVAL.NE.IDF(IIDF)%NODATA)THEN
         SELECT CASE (TRIM(CFUNC))
          CASE ('MEAN','SUM')
           IDF( 0)%X(ICOL,IROW)=IDF( 0)%X(ICOL,IROW)+XVAL
           IDF(-1)%X(ICOL,IROW)=IDF(-1)%X(ICOL,IROW)+1.0
          CASE ('MIN')
           IF(XVAL.LT.IDF(0)%X(ICOL,IROW))THEN
            IDF( 0)%X(ICOL,IROW)=XVAL
            IDF(-1)%X(ICOL,IROW)=IDF(IIDF)%JD
           ENDIF
          CASE ('MAX')
           IF(XVAL.GT.IDF(0)%X(ICOL,IROW))THEN
            IDF( 0)%X(ICOL,IROW)=XVAL
            IDF(-1)%X(ICOL,IROW)=IDF(IIDF)%JD
           ENDIF
          CASE ('PERC')
           IDF(-1)%X(ICOL,IROW)=IDF(-1)%X(ICOL,IROW)+1.0
           XPERC(I)            =XVAL
         END SELECT          
        ENDIF
       ENDIF
      END DO
      IF((TRIM(CFUNC)).EQ.'PERC')THEN
       CALL UTL_GETMED(XPERC,INT(IDF(-1)%X(ICOL,IROW)),-9999.99,(/0.5/),1,I,XMED)
       IDF(0)%X(ICOL,IROW)=XMED(1)
      ENDIF
     ENDIF
    ENDDO
    IF(IBATCH.EQ.0)THEN
     CALL WINDOWSELECT(0)
     CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,IDF(0)%NROW,'Progress NON-Equal IDF"s: ')
    ELSE
     WRITE(*,'(1X,A,F10.2,A)') 'Progress NON-Equal IDF"s: ',REAL(IROW*100)/REAL(IDF(0)%NROW),'%'
    ENDIF
   ENDDO
  ENDIF

  !## close all files
  DO I=1,NFILES
   IIDF=JLIST(I)
   CLOSE(IDF(IIDF)%IU)
  END DO

  SELECT CASE (TRIM(CFUNC))
   CASE ('MEAN')
    DO IROW=1,IDF(0)%NROW
     DO ICOL=1,IDF(0)%NCOL
      IF(IDF(-1)%X(ICOL,IROW).EQ.0.0)THEN
       IDF(0)%X(ICOL,IROW)=IDF(0)%NODATA
      ELSE
       IDF(0)%X(ICOL,IROW)=IDF(0)%X(ICOL,IROW)/IDF(-1)%X(ICOL,IROW)
      ENDIF
     END DO
    END DO
   CASE ('MIN','MAX')
    DO IROW=1,IDF(0)%NROW
     DO ICOL=1,IDF(0)%NCOL
      IF(IDF(-1)%X(ICOL,IROW).EQ.0.0)THEN
       IDF(0)%X(ICOL,IROW)=IDF(0)%NODATA
      ELSE
       I=INT(IDF(-1)%X(ICOL,IROW))
       IDF(-1)%X(ICOL,IROW)=REAL(JDATETOIDATE(I))
      ENDIF
     END DO
    END DO
   CASE ('SUM')
    DO IROW=1,IDF(0)%NROW
     DO ICOL=1,IDF(0)%NCOL
      IF(IDF(-1)%X(ICOL,IROW).EQ.0.0)THEN
       IDF(0)%X(ICOL,IROW)=IDF(0)%NODATA
      ENDIF
     END DO
    END DO
  END SELECT
  
  LINEP='-'
  IF(MEAN_NPERIOD.GT.0)THEN
   LINEP=''
   K=0
   DO J=1,MEAN_NPERIOD/2
    K=K+1
    WRITE(LINE,'(A,1X,I2.2,A,I2.2)') TRIM(LINEP),MEAN_IPERIOD(K,1),'-',MEAN_IPERIOD(K,2)
    LINEP=LINE
    K=K+1
    WRITE(LINE,'(2A,I2.2,A,I2.2)') TRIM(LINEP),'/',MEAN_IPERIOD(K,1),'-',MEAN_IPERIOD(K,2)
    LINEP=LINE
   ENDDO
  ENDIF

  LINE='-'
  IF(MEAN_NYEAR.GT.0)WRITE(LINE,'(99(I4,1X))') (MEAN_IYEAR(I),I=1,MEAN_NYEAR)

  CALL IDFFILLCOMMENT(IDF(0),'Units: Unknown'//NEWLINE// &
                             'Ilay: '//TRIM(ITOS(ILAY))//NEWLINE// &
                             'From Date: '//TRIM(ITOS(MEAN_FYR))//NEWLINE// &
                             'To Date: '//TRIM(ITOS(MEAN_TYR))//NEWLINE// &
                             'Including Years: '//TRIM(LINE)//NEWLINE//&
                             'Including Periods: '//TRIM(LINEP))
  IF(TRIM(CFUNC).EQ.'MEAN')THEN
   CALL IDFFILLCOMMENT(IDF(-1),'Units: Counter'//NEWLINE// &
                               'Ilay: '//TRIM(ITOS(ILAY))//NEWLINE// &
                               'From Date: '//TRIM(ITOS(MEAN_FYR))//NEWLINE// &
                               'To Date: '//TRIM(ITOS(MEAN_TYR))//NEWLINE// &
                               'Including Years: '//TRIM(LINE)//NEWLINE//&
                               'Including Periods: '//TRIM(LINEP))
  ELSE
   CALL IDFFILLCOMMENT(IDF(-1),'Units: Date'//NEWLINE// &
                               'Ilay: '//TRIM(ITOS(ILAY))//NEWLINE// &
                               'From Date: '//TRIM(ITOS(MEAN_FYR))//NEWLINE// &
                               'To Date: '//TRIM(ITOS(MEAN_TYR))//NEWLINE// &
                               'Including Years: '//TRIM(LINE)//NEWLINE//&
                               'Including Periods: '//TRIM(LINEP))
  ENDIF
  
  IF(.NOT.IDFWRITE(IDF(0),MEAN_FMEAN(II),1).OR..NOT.IDFWRITE(IDF(-1),MEAN_FTOTAL(II),1))THEN
   !## error occured
  ENDIF

  !## make cut to fit polygon only
  IF(MEAN_ISEL.EQ.2)THEN
   XMIN= 10.0E10
   YMIN= 10.0E10
   XMAX=-10.0E10
   YMAX=-10.0E10
   DO SHPI=1,SHPNO
    IF(SHPIACT(SHPI).EQ.1.AND.SHPNCRD(SHPI).GT.0)THEN
     XMIN=MIN(XMIN,MINVAL(SHPXC(1:SHPNCRD(SHPI),SHPI)))
     XMAX=MAX(XMAX,MAXVAL(SHPXC(1:SHPNCRD(SHPI),SHPI)))
     YMIN=MIN(YMIN,MINVAL(SHPYC(1:SHPNCRD(SHPI),SHPI)))
     YMAX=MAX(YMAX,MAXVAL(SHPYC(1:SHPNCRD(SHPI),SHPI)))
    ENDIF
   ENDDO
   IF(IDFREAD(IDF(0),MEAN_FMEAN(II),0))THEN
    IF(.NOT.IDFREADPART(IDF(0),XMIN,YMIN,XMAX,YMAX))THEN
    ENDIF
    CLOSE(IDF(0)%IU)
    IF(.NOT.IDFWRITE(IDF(0),MEAN_FMEAN(II),1))THEN
    ENDIF
   ENDIF
   IF(IDFREAD(IDF(-1),MEAN_FTOTAL(II),0))THEN
    IF(.NOT.IDFREADPART(IDF(-1),XMIN,YMIN,XMAX,YMAX))THEN
    ENDIF
    CLOSE(IDF(-1)%IU)
    IF(.NOT.IDFWRITE(IDF(-1),MEAN_FTOTAL(II),1))THEN
    ENDIF
   ENDIF
  ENDIF

  CALL MEAN1ABORT()
 ENDDO

 CLOSE(IU)

 IF(IBATCH.EQ.0)THEN
  CALL WINDOWOPENCHILD(I,FLAGS=HIDEWINDOW+SYSMENUON+MAXBUTTON,TITLE='summary_imod_mean_'//TRIM(OSD_GETENV('USERNAME'))//'txt')
  CALL WEDITFILE(TRIM(MEAN_RESDIR)//'\summary_imod_mean_'//TRIM(OSD_GETENV('USERNAME'))//'txt',MODAL,0,0,COURIERNEW,ISIZE=8)
 ENDIF

 MEAN1COMPUTE =.TRUE.

 END FUNCTION MEAN1COMPUTE

 !###======================================================================
 LOGICAL FUNCTION MEAN1COMPUTE_SUM(IDFNAMES,NFILES,IEXT,IOPTION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NFILES,IEXT,IOPTION
 CHARACTER(LEN=*),DIMENSION(0:NFILES),INTENT(IN) :: IDFNAMES
 INTEGER :: IRAT,IRAT1,I,IC1,IC2,IR1,IR2,IROW,ICOL
 LOGICAL :: LEX
 CHARACTER(LEN=20),DIMENSION(5) :: CFUNC
 DATA CFUNC /'Unknown Operator','Sum Values','Mean Values','Maximal Values','Minimal Values'/

 MEAN1COMPUTE_SUM =.FALSE.

 CALL UTL_MESSAGEHANDLE(0)

 INQUIRE(FILE=IDFNAMES(0),EXIST=LEX)
 IF(LEX)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
   TRIM(IDFNAMES(0))//CHAR(13)//'allready exists overwrite it and continue?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF

 !## nullify idf's
 ALLOCATE(IDF(-1:NFILES)); DO I=-1,NFILES; CALL IDFNULLIFY(IDF(I)); ENDDO

 DO I=1,NFILES
  IF(.NOT.IDFREAD(IDF(I),IDFNAMES(I),0))THEN
   CALL MEAN1ABORT(); RETURN
  ENDIF
 ENDDO

 CALL IDFCOPY(IDF(1),IDF(0))

 !## selected idf's
 IF(IEXT.EQ.2)THEN
  IF(IDF(0)%IEQ.EQ.0)THEN
   IF(.NOT.IDF_EXTENT(NFILES,IDF(1),IDF(0),2))RETURN
  ELSEIF(IDF(0)%IEQ.EQ.1)THEN
   CALL IDFCOPY(IDF(1),IDF(0))
  ENDIF
 !## current zoom window
 ELSEIF(IEXT.EQ.1)THEN
  IF(IDF(0)%IEQ.EQ.0)THEN
   I   =(MPW%XMIN-IDF(1)%XMIN)/IDF(1)%DX
   IDF(0)%XMIN =IDF(1)%XMIN+I*IDF(1)%DX
   I   =(MPW%XMAX-IDF(1)%XMIN)/IDF(1)%DX
   IDF(0)%XMAX =IDF(1)%XMIN+I*IDF(1)%DX
   I   =(IDF(1)%YMAX-MPW%YMIN)/IDF(1)%DY
   IDF(0)%YMIN =IDF(1)%YMAX-I*IDF(1)%DY
   I   =(IDF(1)%YMAX-MPW%YMAX)/IDF(1)%DY
   IDF(0)%YMAX =IDF(1)%YMAX-I*IDF(1)%DY
   CALL UTL_IDFSNAPTOGRID(IDF(0)%XMIN,IDF(0)%XMAX,IDF(0)%YMIN,IDF(0)%YMAX,IDF(0)%DX,IDF(0)%NCOL,IDF(0)%NROW)
  ELSEIF(IDF(0)%IEQ.EQ.1)THEN
   !## make sure idf is within window
   CALL POL1LOCATE(IDF(1)%SX,IDF(1)%NCOL+1,REAL(MPW%XMIN,8),IC1)
   IC1=MAX(1,IC1)
   CALL POL1LOCATE(IDF(1)%SX,IDF(1)%NCOL+1,REAL(MPW%XMAX,8),IC2)
   IC2=MIN(IC2,IDF(1)%NCOL)
   CALL POL1LOCATE(IDF(1)%SY,IDF(1)%NROW+1,REAL(MPW%YMIN,8),IR2)
   IR2=MIN(IR2,IDF(1)%NROW)
   CALL POL1LOCATE(IDF(1)%SY,IDF(1)%NROW+1,REAL(MPW%YMAX,8),IR1)
   IR1=MAX(1,IR1)

   IDF(0)%XMIN=IDF(1)%SX(IC1-1); IDF(0)%XMAX=IDF(1)%SX(IC2)
   IDF(0)%YMIN=IDF(1)%SY(IR2);   IDF(0)%YMAX=IDF(1)%SY(IR1-1)
   IDF(0)%NCOL=IC2-IC1+1;        IDF(0)%NROW=IR2-IR1+1

  ENDIF

 ENDIF  

 CALL IDFCOPY(IDF(0),IDF(-1))
 IF(.NOT.IDFALLOCATEX(IDF(0)).OR..NOT.IDFALLOCATEX(IDF(-1)))THEN
 ENDIF

 SELECT CASE (IOPTION)
  CASE (2,3) !## sum,mean
   IDF(0)%X= 0.0
  CASE (4)   !## max
   IDF(0)%X=-10.0E10
  CASE (5)   !## min
   IDF(0)%X= 10.0E10
 END SELECT
 
 IRAT =0; IRAT1=IRAT
 CALL UTL_WAITMESSAGE(IRAT,IRAT1,0,NFILES,'Progress 0%')
 
 DO I=1,NFILES
  !## scale with blockvalue
  IF(.NOT.IDFREADSCALE_GETX(IDF(I),IDF(-1),10,1,0.0))THEN; CALL MEAN1ABORT(); RETURN; ENDIF
  DO IROW=1,IDF(-1)%NROW; DO ICOL=1,IDF(-1)%NCOL
   SELECT CASE (IOPTION)
    CASE (2,3) !## sum,mean
     IDF(0)%X(ICOL,IROW)=IDF(0)%X(ICOL,IROW)+IDF(-1)%X(ICOL,IROW)
    CASE (4)   !## max
     IDF(0)%X(ICOL,IROW)=MAX(IDF(0)%X(ICOL,IROW),IDF(-1)%X(ICOL,IROW))
    CASE (5)   !## min
     IDF(0)%X(ICOL,IROW)=MAX(IDF(0)%X(ICOL,IROW),IDF(-1)%X(ICOL,IROW))
   END SELECT
  ENDDO; ENDDO
  CALL WINDOWSELECT(0)
  CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,NFILES,'Progress IDF"s: ')
 ENDDO
 SELECT CASE (IOPTION)
  CASE (3)
   IDF(0)%X=IDF(0)%X/REAL(NFILES)
 END SELECT
 
 CALL IDFFILLCOMMENT(IDF(0),'Units: Unknown'//CHAR(13)//'Operator: '//TRIM(CFUNC(IOPTION)))
 IF(.NOT.IDFWRITE(IDF(0),IDFNAMES(0),1))THEN; ENDIF

 CALL MEAN1ABORT()

 MEAN1COMPUTE_SUM =.TRUE.

 END FUNCTION MEAN1COMPUTE_SUM

 !###======================================================================
 SUBROUTINE MEAN1ABORT()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(LISTNAME))DEALLOCATE(LISTNAME)
 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
 IF(ALLOCATED(JLIST))DEALLOCATE(JLIST)
 IF(ALLOCATED(IDF))THEN; CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF); ENDIF
 CALL UTL_MESSAGEHANDLE(1)
 CALL IDFDEALLOCATEX(IDFRP)
 CALL IDFDEALLOCATEX(IDFCP)

 END SUBROUTINE MEAN1ABORT

END MODULE MOD_MEAN_CLC

