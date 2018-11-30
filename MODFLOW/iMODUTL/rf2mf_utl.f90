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

 MODULE RF2MF_UTL
 USE MOD_RF2MF
 USE IMOD_UTL

 PRIVATE

 PUBLIC :: RF2MF_UTL_READNLINES, RF2MF_UTL_FIND_KEYWORD

CONTAINS

 !###====================================================================
 LOGICAL FUNCTION RF2MF_UTL_READNLINES(IMODPCK,IPCK)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODPCK
 INTEGER,INTENT(OUT) :: IPCK
 CHARACTER(LEN=50) :: TXT
 INTEGER :: JMODPCK,I,IOS

 RF2MF_UTL_READNLINES=.FALSE.

 !## read until keyword recognized
 DO
  READ(IURUN,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)THEN
   !## terminate for modules
   IF(IMODPCK.EQ.0)THEN
    CALL IMOD_UTL_PRINTTEXT('',3)
    DO I=1,MXMOD
     IF(RFMOD(I).LT.0)CALL IMOD_UTL_PRINTTEXT(' ('//CMOD(I)//') : present and found ['//TRIM(TXTMOD(I))//']',3,IUOUT)
     IF(RFMOD(I).EQ.0)CALL IMOD_UTL_PRINTTEXT(' ('//CMOD(I)//') : absent ['//TRIM(TXTMOD(I))//']',3,IUOUT)
     IF(RFMOD(I).GT.0)CALL IMOD_UTL_PRINTTEXT(' ('//CMOD(I)//') : present but NOT found! ['//TRIM(TXTMOD(I))//']',3,IUOUT)
    ENDDO
    CALL IMOD_UTL_PRINTTEXT(' Missing data in Module-block',-3)
   !## terminate for packages
   ELSEIF(IMODPCK.EQ.1)THEN
    CALL IMOD_UTL_PRINTTEXT('',3)
    DO I=1,MXPCK
     IF(RFPCK(I).LT.0)CALL IMOD_UTL_PRINTTEXT(' ('//CPCK(I)//') : present and found ['//TRIM(TXTPCK(I))//']',3,IUOUT)
     IF(RFPCK(I).EQ.0)CALL IMOD_UTL_PRINTTEXT(' ('//CPCK(I)//') : absent ['//TRIM(TXTPCK(I))//']',3,IUOUT)
     IF(RFPCK(I).GT.0)CALL IMOD_UTL_PRINTTEXT(' ('//CPCK(I)//') : present but NOT found! ['//TRIM(TXTPCK(I))//']',3,IUOUT)
    ENDDO
    IF(SUM(RFPCK).NE.0)CALL IMOD_UTL_PRINTTEXT(' Missing data in Package-block',-3,IUOUT)
   ENDIF
   !## continue for packages --- EOF could be correct
   IF(IMODPCK.EQ.1)RETURN
  ENDIF
  CALL IMOD_UTL_STRING(LINE)
  READ(LINE,*,IOSTAT=IOS) NLINES,TXT
  IF(IOS.EQ.0)THEN
   CALL IMOD_UTL_S_CAP(TXT,'U')
   IPCK=RF2MF_UTL_FIND_KEYWORD(TXT)
   IF(IPCK.GT.0)JMODPCK=0
   IF(IPCK.LT.0)JMODPCK=1
   IPCK=ABS(IPCK)
   !## proper keyword found AND some data found!
   IF((IPCK.NE.0).AND.(IMODPCK.EQ.JMODPCK))THEN
    IF(IMODPCK.EQ.0)THEN
     IF(RFMOD(IPCK).EQ.1)EXIT    !## module present in header and activated in header
    ENDIF
    IF(IMODPCK.EQ.1)THEN
     IF(RFPCK(IPCK).EQ.1)EXIT    !## package present in header and activated in header
    ENDIF
   ENDIF
  ENDIF
 ENDDO

 IF(IMODPCK.EQ.0)THEN
  !## print error is no data files are found
  IF(MMOD(IPCK).EQ.1.AND.NLINES.EQ.0) &
    CALL IMOD_UTL_PRINTTEXT('Module '//TRIM(TXTMOD(IPCK))//' activated in header, but NO data files found!',-3,IUOUT)
 ELSEIF(IMODPCK.EQ.1)THEN
  !## print warning if no data files are found
  IF(MPCK(IPCK).EQ.1.AND.NLINES.EQ.0)THEN
   CALL IMOD_UTL_PRINTTEXT('',3,IUOUT)
   CALL IMOD_UTL_PRINTTEXT('*** WARNING ***',3,IUOUT)
   CALL IMOD_UTL_PRINTTEXT('Package '//TRIM(TXTPCK(IPCK))//' activated in header, but NO data files found!',3,IUOUT)
   CALL IMOD_UTL_PRINTTEXT('',3,IUOUT)
  ENDIF
 ENDIF

 IF(NLINES.GT.0)THEN
  IF(IMODPCK.EQ.0)THEN
   IF(MMOD(IPCK).EQ.1)THEN
    CALL IMOD_UTL_PRINTTEXT('',3,IUOUT)
    CALL IMOD_UTL_PRINTTEXT('Processing '//TRIM(TXTMOD(IPCK))//' module',3,IUOUT)
   ENDIF
  ENDIF
  IF(IMODPCK.EQ.1)THEN
   IF(MPCK(IPCK).EQ.1)THEN
    CALL IMOD_UTL_PRINTTEXT('',3,IUOUT)
    CALL IMOD_UTL_PRINTTEXT('Processing '//TRIM(TXTPCK(IPCK))//' package',3,IUOUT)
   ENDIF
  ENDIF
 ENDIF

 RF2MF_UTL_READNLINES=.TRUE.

 END FUNCTION RF2MF_UTL_READNLINES

 !#####=================================================================
 INTEGER FUNCTION RF2MF_UTL_FIND_KEYWORD(LINE)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LINE
 INTEGER :: I,J
 CHARACTER(LEN=3) :: CKEY

 RF2MF_UTL_FIND_KEYWORD=0

 I=INDEX(LINE,'(')
 J=INDEX(LINE,')')
 IF(I.EQ.0.OR.J.EQ.0)RETURN
 IF(J-I.NE.4)RETURN

 CKEY=LINE(I+1:J-1)
 CALL IMOD_UTL_S_CAP(CKEY,'U')

 DO I=1,MXMOD
  IF(CKEY.EQ.CMOD(I))EXIT
 END DO
 IF(I.LE.MXMOD)THEN
  RF2MF_UTL_FIND_KEYWORD=I
 ELSE
  DO I=1,MXPCK
   IF(CKEY.EQ.CPCK(I))EXIT
  END DO
  IF(I.LE.MXPCK)RF2MF_UTL_FIND_KEYWORD=-I
 ENDIF
 IF(RF2MF_UTL_FIND_KEYWORD.EQ.0)THEN
  WRITE(*,'(1X,A)') 'LINE:'
  WRITE(*,'(1X,A)') TRIM(LINE)
  WRITE(*,'(1X,A)') 'KEYWORD:'
  WRITE(*,'(1X,A)') CKEY
  WRITE(*,'(/1X,A/)') 'iMODFLOW can not recognize THIS keyword out of:'
  DO I=1,MXMOD
   WRITE(*,'(1X,A)') 'MODULE :'//TRIM(CMOD(I))
  END DO
  DO I=1,MXPCK
   WRITE(*,'(1X,A)') 'PACKAGE:'//TRIM(CPCK(I))
  END DO
  STOP
 ENDIF

 END FUNCTION RF2MF_UTL_FIND_KEYWORD

END MODULE RF2MF_UTL
