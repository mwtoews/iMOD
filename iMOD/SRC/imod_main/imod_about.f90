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
MODULE MOD_ABOUT

USE WINTERACTER
USE RESOURCE
USE IMODVAR
USE MOD_UTL, ONLY : NEWLINE,UTL_GETUNIT,ITOS
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN

CONTAINS

 !###====================================================================
 SUBROUTINE IMODABOUT()
 !###====================================================================
 IMPLICIT NONE

 CALL WDIALOGLOAD(ID_DABOUT)
 CALL WDIALOGPUTSTRING(IDF_STRING1,'About iMOD'//NEWLINE//NEWLINE// &
 'iMOD stands for Interactive MODeling and is a user-friendly interface (iMOD and iMODFLOW) to support the use of (large-scale) '//&
 'groundwater flow models, based upon the concept of MODFLOW. iMOD is specially designed to handle large models that can not be '//&
 'supported by commercial GUIs, such as GMS, Visual Modflow.'//NEWLINE// &
 'iMODs philosophy is to construct on a large scale input files for a variety of parameters that are needed for '// &
 'a groundwater flow model. Instead of focussing on a local area for which a local study needs to be carried out, '// &
 'data is collected '// &
 'for a large area. Within this large area other local models may be constructed in the near future. As a result, the '// &
 'applicability of '// &
 'the model increases, also because various models can be constructed with different grid sizes with local grid refinements. '// &
 'Another '// &
 'simplification compared to the commercial packages is that iMOD uses iMODLOW, restricting the number of different input '// &
 'formats drastically '//&
 'to maximal 4, with one format covering about 90% of all input- and output parameters.'//NEWLINE//NEWLINE// &
 'iMOD Code Architect: dr.ing. Peter (PTM) Vermeulen.')
 CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONTNO,1)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIMOD,1)
 !CALL WDIALOGPUTIMAGE(IDF_PICTURE3,ID_ICONMAIN,1)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE4,ID_ICONOPENGL,1)
 !CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONTNO,1)
 !CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIMOD,1)
 CALL WDIALOGTITLE('About iMOD version '//TRIM(RVERSION))
 CALL WDIALOGPUTSTRING(IDOK,'OK')
 CALL WDIALOGSETFIELD(IDOK)
 CALL WDIALOGSHOW(-1,-1,0,1)

 END SUBROUTINE IMODABOUT

 !###====================================================================
 SUBROUTINE IMODAGREEMENT(CODE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: CODE
 INTEGER :: ITYPE,I,IU,IY,IM,ID
 TYPE(WIN_MESSAGE) :: MESSAGE
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: TEXT
 CHARACTER(LEN=10) :: CDATE

 IF(CODE.EQ.0)THEN
  INQUIRE(FILE=TRIM(PREFVAL(1))//'\license_agreement.txt',EXIST=LEX)
  IF(LEX)THEN
   CODE=1
   RETURN
  ENDIF
 ENDIF

 CALL WDIALOGLOAD(ID_DDISCLAIMER)
 CALL WDIALOGTITLE('User Agreement')
! CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONTNO,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIMOD,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE3,ID_ICONMAIN,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE4,ID_ICONOPENGL,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONTNO,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIMOD,1)

 INQUIRE(FILE=TRIM(PREFVAL(1))//'\license_agreement.txt',EXIST=LEX)
 IF(LEX)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(1))//'\license_agreement.txt',STATUS='OLD',ACTION='READ,DENYWRITE')
  READ(IU,'(A256)') TEXT
  CALL WDIALOGPUTCHECKBOX(IDF_RADIO1,1)
  CALL WDIALOGPUTSTRING(IDF_RADIO1,TEXT)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,3)
  CALL WDIALOGFIELDSTATE(IDOK,3)
  CALL WDIALOGPUTSTRING(IDCANCEL,'Close')
  CLOSE(IU)
 ENDIF

 CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(IMODDISCL()))
 CALL WDIALOGPUTSTRING(IDF_LABEL1,'USER AGREEMENT')
 CALL WDIALOGSETFIELD(IDOK)
 IF(CODE.EQ.1.AND..NOT.LEX)THEN
  CALL WDIALOGFIELDSTATE(IDF_RADIO1,2)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,2)
 ENDIF

 CALL WDIALOGSHOW(-1,-1,0,2)
 I=2
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDOK)
     IF(CODE.EQ.0)CALL WDIALOGGETCHECKBOX(IDF_RADIO1,I)
     EXIT
    CASE (IDCANCEL)
     EXIT
   END SELECT
  END SELECT
 END DO
 CALL WDIALOGUNLOAD()
 IF(I.EQ.1)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(1))//'\license_agreement.txt',STATUS='UNKNOWN',ACTION='WRITE,DENYREAD')
  CALL IOSDATE(IY,IM,ID)
  CDATE=TRIM(ITOS(ID))//'-'//TRIM(ITOS(IM))//'-'//TRIM(ITOS(IY))
  WRITE(IU,'(A)') 'ACCEPTED ON '//TRIM(CDATE)
  WRITE(IU,*)
  WRITE(IU,'(A)') TRIM(IMODDISCL())
  CLOSE(IU)
  CODE=1
 ENDIF

 END SUBROUTINE IMODAGREEMENT

 !###====================================================================
 SUBROUTINE IMODDISCLAIMER()
 !###====================================================================
 IMPLICIT NONE

 CALL WDIALOGLOAD(ID_DABOUT)
 CALL WDIALOGTITLE('Disclaimer iMOD')
 CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(IMODDISCL()))
! CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONTNO,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIMOD,1)

 CALL WDIALOGSETFIELD(IDOK)
 CALL WDIALOGSHOW(-1,-1,0,1)

 END SUBROUTINE IMODDISCLAIMER

 !###====================================================================
 FUNCTION IMODDISCL()
 !###====================================================================
 IMPLICIT NONE

 CHARACTER(LEN=5000) :: IMODDISCL
! IMODDISCL='Copyright (C) Stichting Deltares, 2005-2014.'//NEWLINE//NEWLINE// &
!  'This program is free software: you can redistribute it and/or modify '// &
!  'it under the terms of the GNU General Public License as published by '// &
!  'the Free Software Foundation, either version 3 of the License, or '// &
!  '(at your option) any later version.'//NEWLINE//NEWLINE// &
!  'This program is distributed in the hope that it will be useful, '// &
!  'but WITHOUT ANY WARRANTY; without even the implied warranty of '// &
!  'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the '// &
!  'GNU General Public License for more details.'//NEWLINE//NEWLINE// &
!  'You should have received a copy of the GNU General Public License '// &
!  'along with this program.  If not, see <http://www.gnu.org/licenses/>.'//NEWLINE//NEWLINE// &
!  'Contact: imod.support@deltares.nl'//NEWLINE// &
!  'Stichting Deltares'//NEWLINE// &
!  'P.O. Box 177'//NEWLINE// &
!  '2600 MH Delft, The Netherlands.'

 IMODDISCL='You may use this compiled version of the iMOD-software if you are entitled to this use under a '// &
 'iMOD software license agreement for the iMOD software executables with Deltares or with a party entitled by '// &
 'Deltares to provide sublicenses for the iMOD-software executables. Otherwise use of this compiled version of the '// &
 'iMOD-software is prohibited and illegal. If you are not allowed under a Deltares iMOD license agreement to use the '// &
 'iMOD-software executables, you may find a solution in compiling the open source version of the iMOD-software into an '// &
 'executable yourself (see oss.deltares.nl), or apply for a Deltares iMOD license agreement by sending an email to sales@deltares.nl.'

 END FUNCTION IMODDISCL

 !###====================================================================
 SUBROUTINE IMODSTARTSCREEN()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL :: X

 CALL WDIALOGLOAD(ID_DSTARTSCREEN,ID_DSTARTSCREEN)
 CALL WDIALOGTITLE('iMOD, Interactive Modelling (version: '//TRIM(RVERSION)//')')
! CALL WDIALOGPUTSTRING(IDF_LABEL1,'Deltares Soil & Groundwater Systems'//NEWLINE//NEWLINE//TRIM(IMODDISCL()))
 CALL WDIALOGPUTSTRING(IDF_LABEL1,NEWLINE//TRIM(IMODDISCL()))
! CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONTNO,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIMOD,1)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE3,ID_ICONMAIN,1)
! CALL WDIALOGPUTIMAGE(IDF_PICTURE4,ID_ICONOPENGL,1)
 
 CALL WDIALOGSHOW(-1,-1,0,2)
 CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,1,50)
 DO I=1,50
  CALL RANDOM_NUMBER(X)
  CALL IOSWAIT(INT(X*8.0))
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,I,0)
 END DO
 CALL IOSWAIT(150)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE IMODSTARTSCREEN

END MODULE
