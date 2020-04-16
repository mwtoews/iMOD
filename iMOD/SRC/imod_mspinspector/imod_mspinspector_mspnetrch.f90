!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_MSPNETRCH

USE WINTERACTER
USE RESOURCE
USE MOD_MSPINSPECTOR_PAR
USE MOD_IDF, ONLY : IDFDEALLOCATEX,IDFGETEDGE,IDFGETLOC,IDFGETDXDY,IDFWRITE,IDFCOPY,IDFEQUAL
USE IMODVAR, ONLY : IDIAGERROR
USE MOD_MEAN_PAR, ONLY : MEAN_FYR,MEAN_TYR,MEAN_NYEAR,MEAN_NPERIOD,IBATCH,MEAN_NLAYER, &
                         MEAN_ILAYER,MEAN_FMEAN,MEAN_FTOTAL,MEAN_ISEL,MEAN_RESDIR,CFUNC
USE MOD_MEAN_CLC, ONLY : MEAN1COMPUTE
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_MANAGER_UTL
USE MOD_UTL, ONLY : IDATETOGDATE,ITIMETOGDATE,ITIMETOCDATE,UTL_IDATETOJDATE,DIFFTIME,JD


CONTAINS
 
 !###======================================================================
 LOGICAL FUNCTION MSPNETRCHCOMPUTE() 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FNAME,ROOT
 CHARACTER(LEN=52) :: CDATE
! TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: NYR,YCNT !  Number of Years, YearCounter,yearnumber
 INTEGER :: FYR,FMN,FDY,TYR,TMN,TDY,JD0,JD1,JD2   ! FromYear, FromMonth, FromDay, ToYear, ToMonth, ToDay, 3 x JulianDate
 INTEGER :: I,J,IY,IM,ID,NFILES,IFILES,NU,ICOL,IROW,IDATE,NCREATE,FY,FM,FD,TY,TM,TD
 INTEGER :: IHR,IMT,ISC
 REAL(KIND=DP_KIND) :: TTIME
!LOGICAL :: LEX
 INTEGER(KIND=DP_KIND) :: IDATEFULL
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: IDFNAMES
 INTEGER(KIND=DP_KIND),POINTER,DIMENSION(:) :: ITIME,ITIME_BU
 INTEGER(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: LDATES
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IYEAR

 MSPNETRCHCOMPUTE=.FALSE.

 !## divide full date into separate parts    
 CALL IDATETOGDATE(MSPRCH_FYR,FYR,FMN,FDY) 
 CALL IDATETOGDATE(MSPRCH_TYR,TYR,TMN,TDY)
 JD1=UTL_IDATETOJDATE(MSPRCH_FYR)
 JD2=UTL_IDATETOJDATE(MSPRCH_TYR)
 
 !## get all unique files
 ALLOCATE(ITIME(1000)); ITIME=INT(0,8); NFILES=0
 ROOT=TRIM(SOURCEDIR)//'\HEAD'
 FNAME='HEAD_*_L1.IDF'
 !## get them all (the final time array ITIME)
 IF(UTL_DIRINFO_POINTER(ROOT,FNAME,IDFNAMES,'F',CORDER='N'))THEN; ENDIF
 IF(SIZE(IDFNAMES).EQ.0)THEN 
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No Head files for '//TRIM(FNAME)//' found in '//TRIM(ROOT), 'Error')
   RETURN
 ENDIF
 DO I=1,SIZE(IDFNAMES)
   IDATE=UTL_IDFGETDATE(IDFNAMES(I),IDATEFULL=IDATEFULL) 
   IF(IDATE.NE.0)THEN
    NFILES=NFILES+1
    IF(NFILES.GT.SIZE(ITIME))THEN
     ALLOCATE(ITIME_BU(SIZE(ITIME)+1000))
     DO J=1,SIZE(ITIME); ITIME_BU(J)=ITIME(J); ENDDO
     DEALLOCATE(ITIME); ITIME=>ITIME_BU
    ENDIF
    ITIME(NFILES)=IDATEFULL 
   ENDIF        
 ENDDO
 DEALLOCATE(IDFNAMES)
     
 ALLOCATE(LDATES(NFILES)); DO I=1,NFILES; LDATES(I)=ITIME(I); ENDDO; DEALLOCATE(ITIME)
 
 !## get number unique dates
 CALL UTL_GETUNIQUE_DINT(LDATES,NFILES,NU,0); NFILES=NU

 !## check: HEAD files MUST exist on dayly basis
 TTIME=DIFFTIME(LDATES(1),LDATES(NFILES))+1
 IF(TTIME/NFILES.NE.1)THEN 
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(ITOS(NFILES))//' HEAD Files found for period of '//TRIM(ITOS(INT(TTIME)))//' days. One file for each day expected!', 'Error')
   RETURN
 ENDIF
 
 !## check: Minimum of 2 timesteps must be given
 IF(NFILES.EQ.1)THEN 
  WRITE(*,'(/A)') 'Function is stopped. Only 1 timestep available: '//TRIM(ITOS_DBL(LDATES(1)/INT(1000000,8)))
  WRITE(*,'(A)') 'For NETRCH calculation a minimum of 2 timesteps is needed. '  
  RETURN
 ENDIF
 
 !## create NETRCH folder
 CALL UTL_CREATEDIR(TRIM(RESDIR))

 !## if not given, calc mean Storage Coefficient 
 IF(LEN(TRIM(STOAVG)).EQ.0)THEN 

   !## Calc average storage coefficient
   IBATCH=1
   MEAN_FYR=INT(LDATES(1)/1000000)
   MEAN_TYR=INT(LDATES(NFILES)/1000000)
   CALL IDATETOGDATE(MEAN_FYR,FY,FM,FD) 
   CALL IDATETOGDATE(MEAN_TYR,TY,TM,TD)

   NYR=TY-FY+1
   ALLOCATE(IYEAR(NYR))
   YCNT=0; DO I=FY,TY; YCNT=YCNT+1; IYEAR(YCNT)=I; ENDDO

   MEAN_NLAYER=1
   ALLOCATE(MEAN_ILAYER(MEAN_NLAYER)) ; MEAN_ILAYER(1)=1
   ALLOCATE(MEAN_FMEAN(MAX(1,MEAN_NLAYER)),MEAN_FTOTAL(MAX(1,MEAN_NLAYER)))
   MEAN_ISEL=1
   MEAN_RESDIR=TRIM(SOURCEDIR)//'\METASWAP\MSW_SC1\MSW_SC1'
   CFUNC='MEAN'
   STOAVG =TRIM(MEAN_RESDIR)//'_'//TRIM(CFUNC)//'_'// &                                 
           TRIM(ITOS(FY))//'-'//TRIM(ITOS(FM))//'-'//TRIM(ITOS(FD))//'_to_'// &
           TRIM(ITOS(TY))//'-'//TRIM(ITOS(TM))//'-'//TRIM(ITOS(TD))//'_L'//TRIM(ITOS(MEAN_NLAYER))//'.IDF'
   WRITE(*,*) 'No Mean Storage Coefficient (MSC) file given. MSC is calculated.'  
   WRITE(*,'(A/)') '.... with files: '//TRIM(MEAN_RESDIR)//'_YYYYMMDD_L1.IDF'
   IF(.NOT.MEAN1COMPUTE())THEN ; ENDIF
 ENDIF

CALL IDFNULLIFY(HEAD1) ; CALL IDFNULLIFY(HEAD2) ; CALL IDFNULLIFY(SC1) ; CALL IDFNULLIFY(QMODF)
CALL IDFNULLIFY(NETRCH)

!## read Storage Coefficient
FNAME=STOAVG
IF(.NOT.IDFREAD(SC1,TRIM(FNAME),1))RETURN ; WRITE(*,'(A)') 'Reading '//TRIM(FNAME)//'...'

NCREATE=0
DO IFILES=2,NFILES   ! start at time position 2 while "delta lvgwmodf" is defined as "lvgmodf (t) - lvgmodf (t-1)".
 CALL ITIMETOGDATE(LDATES(IFILES),IY,IM,ID,IHR,IMT,ISC)
 WRITE(CDATE,'(I4.4,2I2.2)') IY,IM,ID
 WRITE(*,*) 'Timestep '//TRIM(CDATE)//'...'  
 JD0=JD(IY,IM,ID)
 !## check if time is within period
 IF(JD0.GE.JD1.AND.JD0.LE.JD2)THEN  
  NCREATE=NCREATE+1

  !## timestep=t  (HEAD2)
  CALL ITIMETOGDATE(LDATES(IFILES),IY,IM,ID,IHR,IMT,ISC)
  WRITE(CDATE,'(I4.4,2I2.2)') IY,IM,ID
  FNAME=TRIM(SOURCEDIR)//'\HEAD\HEAD_'//TRIM(CDATE)//'_L1.IDF'
  IF(.NOT.IDFREAD(HEAD2,TRIM(FNAME),1))RETURN ; WRITE(*,'(A)') 'Reading '//TRIM(FNAME)//'...' 
  FNAME=TRIM(SOURCEDIR)//'\METASWAP\BDGQMODF\BDGQMODF_'//TRIM(CDATE)//'_L1.IDF'
  IF(.NOT.IDFREAD(QMODF,TRIM(FNAME),1))RETURN ; WRITE(*,'(A)') 'Reading '//TRIM(FNAME)//'...'

  !## Initialization
  IF(NCREATE.EQ.1)THEN
    !## create result IDF
    CALL IDFCOPY(HEAD2,NETRCH) 
    !## check whether files are equal
    IF(.NOT.IDFEQUAL(HEAD2,SC1,0))THEN
      WRITE(*,*) 'Error. Size of IDF for Storage Coefficient differs from size of given Model'  
      RETURN
    ENDIF   
  ENDIF

  !## timestep=t-1 (HEAD1)
  CALL ITIMETOGDATE(LDATES(IFILES-1),IY,IM,ID,IHR,IMT,ISC)
  WRITE(CDATE,'(I4.4,2I2.2)') IY,IM,ID

  FNAME=TRIM(SOURCEDIR)//'\HEAD\HEAD_'//TRIM(CDATE)//'_L1.IDF'
  IF(.NOT.IDFREAD(HEAD1,TRIM(FNAME),1))RETURN ; WRITE(*,'(A)') 'Reading '//TRIM(FNAME)//'...'  

  !## calculating NETRCH
  DO IROW=1,HEAD1%NROW
    DO ICOL=1,HEAD1%NCOL
      IF(HEAD1%X(ICOL,IROW).NE.HEAD1%NODATA.OR.HEAD2%X(ICOL,IROW).NE.HEAD2%NODATA.AND. &
          SC1%X(ICOL,IROW).NE.SC1%NODATA.AND.QMODF%X(ICOL,IROW).NE.QMODF%NODATA)THEN
        ! formula is: gwa = delta lvgwmodf * sc1 - qmodf
        NETRCH%X(ICOL,IROW)= ( HEAD2%X(ICOL,IROW) - HEAD1%X(ICOL,IROW) ) * SC1%X(ICOL,IROW) - QMODF%X(ICOL,IROW)
      ELSE
        NETRCH%X(ICOL,IROW)=NETRCH%NODATA
      ENDIF  
    ENDDO
  ENDDO
  
  !## write result IDF
  CALL ITIMETOGDATE(LDATES(IFILES),IY,IM,ID,IHR,IMT,ISC)
  WRITE(CDATE,'(I4.4,2I2.2)') IY,IM,ID
  FNAME=TRIM(RESDIR)//'\NETRCH_'//TRIM(CDATE)//'_L1.IDF' ; WRITE(*,'(A/)') 'Writing '//TRIM(FNAME)//'...' 
  IF(.NOT.IDFWRITE(NETRCH,FNAME,1))THEN ; ENDIF

 ENDIF
ENDDO    

 WRITE(*,'(A)') 'Number of NETRCH files writen: '//TRIM(ITOS(NCREATE))

 MSPNETRCHCOMPUTE=.TRUE.

END FUNCTION MSPNETRCHCOMPUTE
 
END MODULE MOD_MSPNETRCH