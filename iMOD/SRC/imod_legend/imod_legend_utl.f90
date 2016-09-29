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
MODULE MOD_LEGEND_UTL

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : LEGENDOBJ,MXCLASS,MXCLR,MXCGRAD
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_CREATEDIR,UTL_DELSPACE
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG

CONTAINS

 !###======================================================================
 SUBROUTINE LEG_ALLOCATE(LEG)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ) :: LEG

! IF(ASSOCIATED(LEG%LEGTXT))THEN
!  IF(SIZE(LEG%LEGTXT).NE.MXCLR)DEALLOCATE(LEG%LEGTXT)
! ENDIF
! IF(.NOT.ASSOCIATED(LEG%LEGTXT))ALLOCATE(LEG%LEGTXT(MXCLR))

! IF(ASSOCIATED(LEG%CGRAD))THEN
!  IF(SIZE(LEG%CGRAD).NE.MXCGRAD)DEALLOCATE(LEG%CGRAD)
! ENDIF
! IF(.NOT.ASSOCIATED(LEG%CGRAD))ALLOCATE(LEG%CGRAD(MXCGRAD))

! IF(ASSOCIATED(LEG%RGB))THEN
!  IF(SIZE(LEG%RGB).NE.MXCLR)DEALLOCATE(LEG%RGB)
! ENDIF
! IF(.NOT.ASSOCIATED(LEG%RGB))ALLOCATE(LEG%RGB(MXCLR))

! IF(ASSOCIATED(LEG%CLASS))THEN
!  IF(SIZE(LEG%CLASS).NE.MXCLR+1)DEALLOCATE(LEG%CLASS)
! ENDIF
! IF(.NOT.ASSOCIATED(LEG%CLASS))ALLOCATE(LEG%CLASS(0:MXCLR))

 LEG%HEDTXT  =''
 LEG%LEGTXT  =''
 LEG%RGB     =0
 LEG%CLASS   =0.0
 LEG%NCLR    =0
 LEG%CGRAD   =0
 LEG%ICLRGRAD=0
 LEG%LEGSIZE =0

 END SUBROUTINE LEG_ALLOCATE

 !###======================================================================
 SUBROUTINE LEG_DEALLOCATE(LEG)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ) :: LEG

! IF(ASSOCIATED(LEG%LEGTXT))DEALLOCATE(LEG%LEGTXT)
! IF(ASSOCIATED(LEG%CGRAD))DEALLOCATE(LEG%CGRAD)
! IF(ASSOCIATED(LEG%RGB))DEALLOCATE(LEG%RGB)
! IF(ASSOCIATED(LEG%CLASS))DEALLOCATE(LEG%CLASS)

 END SUBROUTINE LEG_DEALLOCATE

 !###======================================================================
 SUBROUTINE LEG_WRITE(LEG,LEGNAME)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ) :: LEG
 CHARACTER(LEN=*)   :: LEGNAME
 INTEGER :: I,IU,IOS

 I=INDEXNOCASE(LEGNAME,'\',.TRUE.)-1
 CALL UTL_CREATEDIR(LEGNAME(:I))

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(LEGNAME),STATUS='REPLACE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not WRITE legend file, no permission!','ERROR')
  RETURN
 ENDIF

 CALL LEG_WRITE_LEGEND(IU,LEG)

 CLOSE(IU)

 END SUBROUTINE LEG_WRITE

 !###======================================================================
 SUBROUTINE LEG_WRITE_LEGEND(IU,LEG)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ) :: LEG
 INTEGER,INTENT(IN) :: IU
 INTEGER :: I,IR,IG,IB
 CHARACTER(LEN=256) :: LINE
 
 !## constant value
 IF(LEG%NCLR.EQ.0)THEN
  LEG%NCLR=1
  LEG%CLASS(1)=LEG%CLASS(0)-0.5
  LEG%CLASS(0)=LEG%CLASS(0)+0.5
 ENDIF
 LINE=TRIM(ITOS(LEG%NCLR))
 DO I=1,SIZE(LEG%CGRAD)
  LINE=TRIM(LINE)//','//TRIM(ITOS(LEG%CGRAD(I)))
 ENDDO
 WRITE(IU,'(A)') TRIM(LINE)
 WRITE(IU,'(A)') 'UPPERBND,LOWERBND,IRED,IGREEN,IBLUE,DOMAIN'
 DO I=1,LEG%NCLR
  CALL WRGBSPLIT(LEG%RGB(I),IR,IG,IB)
!  IF(LEG%NCLR.GT.MXCLASS)THEN
!   IF(I.EQ.1)WRITE(LINE,*) '>=',LEG%CLASS(I),' - <=',LEG%CLASS(I-1)
!   IF(I.NE.1)WRITE(LINE,*) '>=',LEG%CLASS(I),' - <=',LEG%CLASS(I-1)
!  ENDIF    
  LINE=TRIM(RTOS(LEG%CLASS(I-1),'G',7))//','//TRIM(RTOS(LEG%CLASS(I-1),'G',7))//','// &
       TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','//TRIM(ITOS(IB))//',"'//TRIM(LEG%LEGTXT(I))//'"'
!  WRITE(L1,*) LEG%CLASS(I-1),',',LEG%CLASS(I),',',IR,',',IG,',',IB,',"'//TRIM(LEG%LEGTXT(I))//'"'
!  !## delete empty spaces outside quotes only
!  CALL UTL_DELSPACE(L1,LINE) 
  WRITE(IU,'(A)') TRIM(LINE)
 END DO

 END SUBROUTINE LEG_WRITE_LEGEND

 !###======================================================================
 SUBROUTINE LEG_READ(LEG,LEGNAME,IOS)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ) :: LEG
 INTEGER,INTENT(OUT) :: IOS
 CHARACTER(LEN=*),INTENT(IN) :: LEGNAME
 INTEGER :: IU
 CHARACTER(LEN=50) :: CERROR

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(LEGNAME),STATUS='OLD',FORM='FORMATTED',IOSTAT=IOS,ACTION='READ,DENYWRITE')
 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,CERROR)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not READ legend file:'//CHAR(13)// &
   TRIM(LEGNAME)//CHAR(13)//'Error code: ['//TRIM(CERROR)//']','ERROR')
  RETURN
 ENDIF

 CALL LEG_READ_LEGEND(IU,LEG)

 CLOSE(IU)

 END SUBROUTINE LEG_READ

 !###======================================================================
 SUBROUTINE LEG_READ_LEGEND(IU,LEG)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ) :: LEG
 INTEGER,INTENT(IN) :: IU
 INTEGER :: I,IR,IG,IB

 READ(IU,*) LEG%NCLR,LEG%CGRAD(1:SIZE(LEG%CGRAD))
 LEG%CGRAD(1)      =1
 LEG%CGRAD(SIZE(LEG%CGRAD))=1
 READ(IU,*)

 LEG%CLASS=0.0
 DO I=1,LEG%NCLR
  READ(IU,*) LEG%CLASS(I-1),LEG%CLASS(I),IR,IG,IB,LEG%LEGTXT(I)
  LEG%RGB(I)=WRGB(IR,IG,IB)
 END DO

 END SUBROUTINE LEG_READ_LEGEND

END MODULE MOD_LEGEND_UTL

