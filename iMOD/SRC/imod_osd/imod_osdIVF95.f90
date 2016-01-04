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
MODULE MOD_OSD

USE WINTERACTER

INTEGER,PARAMETER :: ICF=1  !## operating is zero=lahey90 1=intel95

INTEGER,PARAMETER :: NOS=3
INTEGER,SAVE :: OS = 1                      !## operating system 1=dos,2=linux,3=unix
CHARACTER(LEN=20),DIMENSION(NOS),SAVE :: OSN

CONTAINS

 !###===================================================================
 SUBROUTINE UTL_OSSYSTEM()
 !###===================================================================
 IMPLICIT NONE
 INTEGER :: VOS,OSD_GET_OS

 !#get operating system
 VOS=OSD_GET_OS()
 OS =0
 IF(VOS.EQ.3)OS=1
 IF(VOS.EQ.2)OS=2
 IF(VOS.EQ.4)OS=2

 OS=1

 SELECT CASE (OS)
  !## dos
  CASE (1)
   OSN(OS)   ='DOS-mode'
  !## linux/unix (beowulf)
  CASE (2)
   OSN(OS)   ='UNIX/LINUX-mode'
  !## something different
  CASE DEFAULT
   WRITE(*,*) 'No proper operating system!'
 END SELECT

 END SUBROUTINE UTL_OSSYSTEM

!###======================================================================
 SUBROUTINE OSD_GETARG(NARG,STRING)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NARG
 CHARACTER(LEN=*),INTENT(OUT) :: STRING

 CALL GETARG(NARG,STRING)

 END SUBROUTINE OSD_GETARG

 !###======================================================================
 SUBROUTINE OSD_GETNARG(NARG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: NARG

 NARG=COMMAND_ARGUMENT_COUNT()

 END SUBROUTINE OSD_GETNARG
 
 !###======================================================================
 SUBROUTINE OSD_DATE_AND_TIME(DATEANDTIME,IDATE,ITIME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,OPTIONAL,INTENT(OUT) :: IDATE,ITIME
 CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: DATEANDTIME
 CHARACTER(LEN=50) :: CTIME
 INTEGER :: TIME
 
 IF(PRESENT(IDATE))PAUSE
 IF(PRESENT(ITIME))PAUSE
 IF(PRESENT(DATEANDTIME))THEN
  DATEANDTIME=CTIME(TIME())
  DATEANDTIME=DATEANDTIME(1:LEN_TRIM(DATEANDTIME)-1) !## there is something "dirty" on the back of this
 ENDIF

 END SUBROUTINE OSD_DATE_AND_TIME

 !###======================================================================
 SUBROUTINE OSD_TIMER(ISEC) 
 !## gives hundreds of a second since midnight
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: SM=60*100    !## hundreds of a second/minute
 INTEGER,PARAMETER :: SH=60*SM     !## second/hour
 INTEGER,INTENT(OUT) :: ISEC
 INTEGER :: HH,MM,SS,MS

 CALL IOSTIME(HH,MM,SS,MS)
 MS=MS/10 !## thousands of a seconds -> hundreds of a second
 ISEC=(MAX(0,HH-1))*SH + (MAX(0,MM-1))*SM + (MAX(0,SS-1))*100  + MS

 END SUBROUTINE OSD_TIMER

 !###======================================================================
 SUBROUTINE OSD_IOSTAT_MSG(IOS,MSG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOS
 CHARACTER(LEN=*),INTENT(OUT) ::MSG

 MSG='Error syntaxt Not supported from IntelVisualFortran!'
 !CALL OSD_IOSTAT_MSG(IOS,MSG)
 
 END SUBROUTINE OSD_IOSTAT_MSG

 !###======================================================================
 FUNCTION OSD_GETENV(IKEYW)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IKEYW
 CHARACTER(LEN=50) :: OSD_GETENV
 
 CALL GETENV(IKEYW,OSD_GETENV)

 END FUNCTION OSD_GETENV

 !###======================================================================
 SUBROUTINE OSD_OPEN(IU,FILE,STATUS,FORM,ACTION,ACCESS,POSITION,RECL,IOSTAT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: IU
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: FILE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: STATUS,FORM,ACTION,ACCESS,POSITION
 INTEGER,INTENT(IN),OPTIONAL :: RECL
 INTEGER,INTENT(OUT),OPTIONAL :: IOSTAT
 CHARACTER(LEN=25) :: TSTAT,TFORM,TACTION,TACCESS,TPOS
 INTEGER :: TRECL,IOS

 !## get valid unitnumber
 IF(IU.LE.0)THEN
  IU=0; CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No valid unit number given (IU.LE.0)!','Error')
  RETURN
 ENDIF
 
 TSTAT  ='UNKNOWN'
 TFORM  ='FORMATTED'
 TACTION='READWRITE'   !## default
 TACCESS='SEQUENTIAL'
 TRECL  = 0
 TPOS   ='REWIND'

 IF(PRESENT(STATUS))  TSTAT  =STATUS
 IF(PRESENT(FORM))    TFORM  =FORM
 IF(PRESENT(ACTION))  TACTION=ACTION
 IF(PRESENT(ACCESS))  TACCESS=ACCESS
 IF(PRESENT(POSITION))TPOS   =POSITION
 IF(PRESENT(RECL))    TRECL=RECL
 
 !## make sure parameters do not conflict
 IF(INDEX(TACTION,'DENY').GT.0)THEN
  IF(TACTION.EQ.'DENYNONE')TACTION='READWRITE'
  IF(TACTION.EQ.'READ,DENYWRITE')TACTION='READ'
  IF(TACTION.EQ.'WRITE,DENYREAD')TACTION='WRITE'
 ENDIF
 IF(INDEX(TACCESS,'TRANSPARENT').GT.0)THEN
  TACCESS='STREAM'
 ENDIF
 
 IF(TRECL.EQ.0)OPEN(IU,FILE=FILE,STATUS=TSTAT,FORM=TFORM,ACTION=TACTION,ACCESS=TACCESS,POSITION=TPOS,IOSTAT=IOS)
 IF(TRECL.GT.0)OPEN(IU,FILE=FILE,STATUS=TSTAT,FORM=TFORM,ACTION=TACTION,ACCESS=TACCESS,RECL    =TRECL,IOSTAT=IOS)
 
 IF(IOS.NE.0)IU=0; IF(PRESENT(IOSTAT))IOSTAT=IOS

 IF(IU.LE.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot open file:'//CHAR(13)//TRIM(FILE),'Error')

 END SUBROUTINE OSD_OPEN

END MODULE MOD_OSD
