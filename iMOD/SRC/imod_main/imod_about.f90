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
MODULE MOD_ABOUT

USE WINTERACTER
USE RESOURCE
USE IMODVAR
USE MOD_UTL, ONLY : NEWLINE,UTL_GETUNIT,ITOS
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN

CHARACTER(LEN=32) :: LICFILE='I_accepted.txt'
CHARACTER(LEN=:),ALLOCATABLE :: STR

CONTAINS

 !###====================================================================
 SUBROUTINE IMOD_ABOUT()
 !###====================================================================
 IMPLICIT NONE

 CALL WDIALOGLOAD(ID_DABOUT)
 CALL WDIALOGPUTSTRING(IDF_STRING1,'About iMOD'//NEWLINE//NEWLINE// &
 'iMOD: a new framework for iMODFLOW and geo-modeling'//NEWLINE//NEWLINE// &
 'iMOD is an easy to use Graphical User Interface + an accelerated Deltares-version of MODFLOW with fast, flexible and consistent sub-domain modeling techniques. iMOD facilitates very large, high resolution groundwater modeling and also geo-editing of the subsurface.'//NEWLINE//NEWLINE// &
 'WHY iMOD? iMOD is a fast groundwater modeling environment that allows to build large high resolution groundwater flow models based on a data set expandable to all possible future areas of interest:'//NEWLINE// &
 ' •  flexibility to generate high or low resolution models everywhere when needed;'//NEWLINE// &
 ' •  generate sub-domain models of any part of the area covered by your data;'//NEWLINE// &
 ' •  maintain consistency between regional and inlying sub-domain models;'//NEWLINE// &
 ' •  update your data set with the details added in a sub-domain model.'//NEWLINE//NEWLINE// &
 'See for more information the open-source website:'//NEWLINE// &
 'see http://oss.deltares.nl/web/iMOD.'//NEWLINE//NEWLINE// & 
 'iMOD Code Architect: dr. Peter (P.T.M.) Vermeulen.')
 CALL WDIALOGPUTIMAGE(IDF_PICTURE1,ID_ICONTNO,1)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE2,ID_ICONIMOD,1)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE4,ID_ICONOPENGL,1)
 CALL WDIALOGTITLE('About iMOD V'//TRIM(RVERSION))
 CALL WDIALOGPUTSTRING(IDOK,'OK')
 CALL WDIALOGSETFIELD(IDOK)
 CALL WDIALOGSHOW(-1,-1,0,1)

 END SUBROUTINE IMOD_ABOUT

 !###====================================================================
 SUBROUTINE IMOD_AGREEMENT(CODE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: CODE
 INTEGER :: ITYPE,I,IU,IY,IM,ID
 TYPE(WIN_MESSAGE) :: MESSAGE
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: TEXT
 CHARACTER(LEN=10) :: CDATE

 IF(CODE.EQ.0)THEN
  INQUIRE(FILE=TRIM(PREFVAL(1))//'\'//TRIM(LICFILE),EXIST=LEX)
  IF(LEX)THEN; CODE=1; RETURN; ENDIF
 ENDIF

 CALL WDIALOGLOAD(ID_DDISCLAIMER)
 CALL WDIALOGTITLE('iMOD Software License Agreement')

 INQUIRE(FILE=TRIM(PREFVAL(1))//'\'//TRIM(LICFILE),EXIST=LEX)
 IF(LEX)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(1))//'\'//TRIM(LICFILE),STATUS='OLD',ACTION='READ,DENYWRITE')
  READ(IU,'(A256)') TEXT
  CALL WDIALOGPUTCHECKBOX(IDF_RADIO1,1)
  CALL WDIALOGPUTSTRING(IDF_RADIO1,TEXT)
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,3)
  CALL WDIALOGFIELDSTATE(IDOK,3)
  CALL WDIALOGPUTSTRING(IDCANCEL,'Close')
  CLOSE(IU)
 ENDIF

 CALL IMOD_PUTLICENSE(IDF_STRING1)
 CALL WDIALOGPUTSTRING(IDF_LABEL1,'User Agreement')
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
  CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(1))//'\'//TRIM(LICFILE),STATUS='UNKNOWN',ACTION='WRITE,DENYREAD')
  CALL IOSDATE(IY,IM,ID); CDATE=TRIM(ITOS(ID))//'-'//TRIM(ITOS(IM))//'-'//TRIM(ITOS(IY))
  WRITE(IU,'( A )') 'You accepted the term and conditions of the iMOD Software License Agreement on '//TRIM(CDATE)
  CALL IMOD_PUTLICENSE(-IU)
  CODE=1
 ENDIF

 END SUBROUTINE IMOD_AGREEMENT

 !###====================================================================
 SUBROUTINE IMOD_STARTLICENSE(ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER,PARAMETER :: STRLEN=2000
 INTEGER :: IU,I
 CHARACTER(LEN=256) :: TEXT
 LOGICAL :: LEX
 
 ALLOCATE(CHARACTER(LEN=STRLEN) :: STR)

 STR='Copyright (C) Stichting Deltares, 2005-2015. '//NEWLINE// &
NEWLINE// &
'This Deltares-software executable is part of iMOD. iMOD is Deltares-soft'// &
'ware; the source code of iMOD is also available as free open source soft'// &
'ware at oss.deltares.nl. You may use the Deltares-software executables o'// &
'f iMOD without any remuneration to be paid to Deltares if you accepted t'// &
'he iMOD Software License Agreement (iMOD License) which is offered to yo'// &
'u as a PDF-file; you should have received a copy of this PDF-file with t'// &
'his Deltares-software '//NEWLINE// &
'executable. If not, see '//NEWLINE// &
'http://oss.deltares.nl/web/iMOD/iMOD_Software_License_Agreement. '//NEWLINE// &
NEWLINE// &
'According to the file "I_accepted.txt" on your computer you accepted the'// &
' terms and conditions of the iMOD license on <date and time>; WARNING: I'// &
'F IT WAS NOT YOU OR THE LEGAL ENTITY ON WHOSE BEHALF YOU INTENT TO USE T'// &
'HE IMOD-EXECUTABLE, THAT ACCEPTED THE TERMS AND CONDITIONS OF THE iMOD L'// &
'ICENSE YOU ARE NOT ENTITLED TO USE THIS DELTARES-EXECUTABLE OF IMOD. In '// &
'this case your use of this Deltares-executable of the iMOD-software is p'// &
'rohibited and illegal: abort the use of this Deltares-executable immedia'// &
'tely and refrain from using the Deltares-executables of iMOD. To make us'// &
'e of the Deltares-executables of iMOD please make sure to accept the ter'// &
'ms and conditions or have it lawfully accepted by the legal entity on wh'// &
'ose behalf you intent to use the iMOD-executable by re-invoking the ‘I a'// &
'ccept’-procedure; to re-invoke the "I accept"-procedure abort the use of'// &
' this Deltares-executable of iMOD, delete the file "I_accepted.txt", and'// &
' invoke this Deltares-executable of iMOD again.'//NEWLINE// &
NEWLINE// &
'The iMOD software is distributed in the hope that it will be useful, but'// &
' WITHOUT ANY GUARANTEE OR (IMPLIED) WARRANTY. Any use of the Deltares-ex'// &
'ecutables of the iMOD-software is for your own risk. See the iMOD Licens'// &
'e for more details.'//NEWLINE// &
NEWLINE// &
'For more info, please contact: Stichting Deltares, P.O. Box 177, 2600 MH'// &
' Delft,The Netherlands. Email: imod.support@deltares.nl. '
 
 INQUIRE(FILE=TRIM(PREFVAL(1))//'\'//TRIM(LICFILE),EXIST=LEX)
 IF(LEX)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(1))//'\'//TRIM(LICFILE),STATUS='OLD',ACTION='READ,DENYWRITE')
  READ(IU,'(79X,A)') TEXT
  CLOSE(IU)
  I=INDEX(STR,'<date and time>')
  IF(I.GT.0)THEN
   WRITE(STR(I+1:I+13),'(A12)') ' '//TRIM(TEXT)//' '
  ENDIF
 ENDIF
 
!
  CALL WDIALOGPUTSTRING(ID,TRIM(STR))
 
 DEALLOCATE(STR)
 
 END SUBROUTINE IMOD_STARTLICENSE

 !###====================================================================
 SUBROUTINE IMOD_PUTLICENSE(ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER,PARAMETER :: STRLEN=20000 

 ALLOCATE(CHARACTER(LEN=STRLEN) :: STR)
 
STR='iMOD Software License Agreement'//NEWLINE// &
NEWLINE// &
'This is a license for the iMOD-executables, distributed from the Stichti'// &
'ng Deltares website. If you require a printed version of this iMOD Softw'// &
'are License Agreement, e.g. prior to accepting these terms and condition'// &
's, please print the pdf-file ‘iMOD_Software_License_Agreement_V3_01.pdf’. You '// &
'should have received a copy of this pdf-file; if not, see '//NEWLINE// &
'http://oss.deltares.nl/web/iMOD/iMOD_software_license_agreement.'//NEWLINE// &
NEWLINE// &
'Read this software license agreement'//NEWLINE// &
NEWLINE// &
'General'//NEWLINE// &
NEWLINE// &
'This is a legal agreement between the prospective user, either an indivi'// &
'dual or an entity, (hereafter: “Licensee”) and Stichting Deltares (herea'// &
'fter “Deltares”) to license to Licensee the computer program called iMOD'// &
' as described below under “DESCRIPTION OF iMOD” (hereafter “iMOD”). '//NEWLINE// &
'By marking the “Yes, I Accept”-checkbox in the iMOD-GUI or entering “Y” '// &
'or “y” when prompted in iMODFLOW:'//NEWLINE// &
NEWLINE// &
'1. You expressly declare being authorized to act on behalf of Licensee f'// &
'or the purposes of accepting this software license agreement (hereafter '// &
'“License Agreement”);'//NEWLINE// &
'2. Licensee expressly accepts this License Agreement and accepts to be l'// &
'egally bound by the terms and conditions contained therein. '//NEWLINE// &
NEWLINE// &
'If you are not authorized to act on behalf of Licensee to agree upon thi'// &
's License Agreement, please do not mark the “Yes, I accept”-checkbox in '// &
'the iMOD-GUI and do not enter “Y” or “y” when prompted from iMODFLOW and '// &
'exit the iMOD-program by clicking the “Cancel” checkbox in the iMOD-GUI '// &
'and exit the iMOD-GUI and do not enter “Y” or “y” when prompted in iMODFL'// &
'OW and exit iMODFLOW by hitting the enter-key. Furthermore do not mark th'// &
'e “Yes, I Accept”-checkbox in the iMOD-GUI or enter “Y” or “y” when prom'// &
'pted in iMODFLOW and end iMOD if Licensee does not agree with the License'// &
' Agreement. '//NEWLINE// &
NEWLINE// &
'WHEREAS:'//NEWLINE// &
'Deltares owns the intellectual property of the computer program develope'// &
'd by Deltares, including documentation, as described below under DESCRIP'// &
'TION OF iMOD, hereinafter referred to as “iMOD”;'//NEWLINE// &
'Licensee wishes to acquire a non-exclusive and non-transferable license,'// &
' without the right of sub-licensing, to use iMOD within Licensee‘s organ'// &
'isation;'//NEWLINE// &
'Deltares grants Licensee an iMOD-license on the following conditions.'//NEWLINE// &
NEWLINE// &
'AGREE AS FOLLOWS:'//NEWLINE// &
NEWLINE// &
'Article 1 License'//NEWLINE// &
NEWLINE// &
'Deltares grants Licensee a conditional, non-exclusive, non-transferable '// &
'license without the right to sublicense or modify, to use iMOD within th'// &
'e organisation of Licensee. This License Agreement is provided solely un'// &
'der the condition that Licensee complies with the provisions of this Lic'// &
'ense Agreement. '//NEWLINE// &
NEWLINE// &
'Article 2 Use of iMOD (including the documentation) '//NEWLINE// &
NEWLINE// &
'1. Licensee shall only be authorised to use iMOD within its own organisa'// &
'tion and for its own use. Licensee shall not be permitted to make any ot'// &
'her use of iMOD or to make available or grant access to iMOD to any thir'// &
'd party.'//NEWLINE// &
'2. Licensee shall not be authorised to modify and/or adjust iMOD and/or '// &
'to (otherwise) carry out alterations to it and/or to integrate iMOD in o'// &
'ther software, unless and only in so far as Licensee has obtained expres'// &
's written permission to that effect in advance from Deltares. iMOD may –'// &
' furthermore – only be used on the hardware platform & operating system '// &
'and the system software as defined below under OTHER REQUIREMENTS FOR TH'// &
'E USE OF iMOD or on alternatives for the hardware platform & operating s'// &
'ystem and/or the System Software that have been approved by Deltares in '// &
'writing.'//NEWLINE// &
'3. Licensee shall not be authorised to (have others) copy iMOD in any ma'// &
'nner whatsoever or to (have others) multiply it (in any other way), exce'// &
'pt for backup purposes. '//NEWLINE// &
NEWLINE// &
'Article 3 Intellectual Property Rights, Ownership '//NEWLINE// &
NEWLINE// &
'1. All rights, including the intellectual property rights, to iMOD and d'// &
'ocumentation are owned by Deltares. Licensee acknowledges that this Lice'// &
'nse Agreement does not provide Licensee with any rights or ownership to '// &
'iMOD or documentation, including any rights to the intellectual property'// &
'.'//NEWLINE// &
'2. All changes to iMOD developed (or to be developed in the future) by D'// &
'eltares will remain the intellectual property of Deltares. In so far as '// &
'Licensee obtains any intellectual property to these features or function'// &
'alities (other than the right to use such changes under this license), L'// &
'icensee will transfer all intellectual property concerning the above-men'// &
'tioned feature(s) and/or functionalities to Deltares.'//NEWLINE// &
'3. All developments to iMOD are always intended to be distributed by Del'// &
'tares to all licensees. Deltares shall not bind herself in any contract '// &
'or whatsoever to limit the distribution of new developments of iMOD.'//NEWLINE// &
'4. Deltares represents and warrants that to the best of its knowledge iM'// &
'OD does not infringe on third party intellectual property rights.'//NEWLINE// &
NEWLINE// &
'Article 4 Confidentiality'//NEWLINE// &
NEWLINE// &
'1. Licensee shall keep confidential iMOD which Licensee has obtained and'// &
'/or obtains, in any manner, from Deltares under or in connection with th'// &
'e License Agreement. '//NEWLINE// &
'This obligation shall at any rate include: '//NEWLINE// &
'a. '//NEWLINE// &
'Treating of iMOD confidentially; '//NEWLINE// &
'b.'//NEWLINE// &
'releasing iMOD solely to those employees of Licensees under  the conditi'// &
'ons of this License Agreement who require access to iMOD, whereby Licens'// &
'ee will oblige these employees of the same confidentiality as Licensee;'//NEWLINE// &
'c.'//NEWLINE// &
'The non-disclosure of information and/or data related to the License Agr'// &
'eement to third parties and/or refraining from making such information a'// &
'nd/or data public in any other way without the prior express and written'// &
' consent of Deltares, to be obtained for each separate event. '//NEWLINE// &
'd.'//NEWLINE// &
'Using information and/or data obtained solely for the purposes for which'// &
' they were obtained.'//NEWLINE// &
NEWLINE// &
'2. Licensee‘s obligation of confidentiality referred to in Article 4.1 s'// &
'hall not apply to information and/or data that were already at Licensee‘'// &
's free disposal, or were part of the public domain, or were already incl'// &
'uded in generally accessible literature at the time when they were obtai'// &
'ned by Licensee, or that were obtained by Licensee from a third party or'// &
' third parties who was or were free to disclose the relevant information'// &
' and/or data and who had not obtained the information and/or data from D'// &
'eltares.'//NEWLINE// &
NEWLINE// &
'Article 5  No guarantee, no warrantee '//NEWLINE// &
NEWLINE// &
'Deltares has developed, or produced, as the case may be, iMOD to the bes'// &
't of its abilities and in accordance with the state of art. However, Del'// &
'tares does not give any guarantee or warrantee with respect to iMOD or i'// &
'ts functioning and the contents thereof and/or the results obtained or t'// &
'o be obtained with iMOD, or the correctness or quality thereof.  '//NEWLINE// &
NEWLINE// &
'Article 6  Duration, Termination '//NEWLINE// &
NEWLINE// &
'1. This License Agreement is concluded for an indefinite period, subject'// &
' to termination in accordance with the provisions of article 6.2 and 6.3'// &
'. Except based on these provisions, parties are not allowed to terminate'// &
' the License Agreement. '//NEWLINE// &
'2. Without prejudice to their rights to receive compensation, parties ar'// &
'e entitled to terminate the License Agreement  in writing with immediate'// &
' effect, without judicial intervention being required, if the other part'// &
'y fails to comply, or to comply timely or fully, with its obligations un'// &
'der the License Agreement, provided that the defaulting party shall be g'// &
'ranted one month’s time to comply with its obligations as yet, in which '// &
'event the License Agreement shall continue to be in force.'//NEWLINE// &
'3. Deltares shall be entitled to terminate the License Agreement forthwi'// &
'th in writing with immediate effect, without judicial intervention being'// &
' required, if Licensee is adjudged bankrupt, is granted a moratorium on '// &
'payments, is dissolved or liquidated, or if (an) application(s) to this '// &
'end  has (have) been filled.'//NEWLINE// &
'4. In the event of termination of the License Agreement, Licensee shall '// &
'immediately uninstall and remove iMOD from its system(s). '//NEWLINE// &
'5. The following provisions shall remain in full force after termination'// &
' of the License Agreement as set forth in this Article: Article 3.2, Art'// &
'icle 4 and Article 7. '//NEWLINE// &
NEWLINE// &
'Article 7  Liability'//NEWLINE// &
NEWLINE// &
'1. Licensee agrees that Deltares (including its personnel and non-employ'// &
'ees who (have) undertake(n) activities for Deltares) shall not be respon'// &
'sible to Licensee for any loss-of-profit, direct, indirect, incidental, '// &
'special or consequential damages arising out of the License Agreement or'// &
' the installation or the use of iMOD, except for damages caused by wilfu'// &
'l act/conduct or gross negligence of Deltares or its personnel.'//NEWLINE// &
'2. Licensee shall indemnify, hold harmless and defend Deltares against a'// &
'ny action brought by a third party against Deltares to the extent that s'// &
'uch a claim is connected to the use of iMOD by Licensee and/or third par'// &
'ties at whose disposal the Licensee has placed iMOD in accordance with t'// &
'his License Agreement and/or these results or to whom he has otherwise m'// &
'ade them known the results, including use of the results of use by Licen'// &
'see and/or third parties.'//NEWLINE// &
NEWLINE// &
'Article 8  Other provisions'//NEWLINE// &
NEWLINE// &
'1. Licensee is not allowed to assign any rights and/or obligations under'// &
' the License Agreement, entirely or in part, to third parties without th'// &
'e prior written consent of Deltares.'//NEWLINE// &
'2. Any disputes arising from the License Agreement or from agreements ar'// &
'ising therefrom, shall be submitted solely to the competent court of The'// &
' Hague.'//NEWLINE// &
'3. This License Agreement and all the agreements arising therefrom are g'// &
'overned exclusively by Netherlands law.'//NEWLINE// &
NEWLINE// &
'DESCRIPTION OF iMOD'//NEWLINE// &
NEWLINE// &
'This iMOD Software License Agreement contains the following executables (<xx>: bugfix-versionnumber):'// &
NEWLINE// &
NEWLINE// &
'- The iMOD Graphical User Interface (iMOD-GUI): iMOD_V3_01_<xx>_X32R.exe and i'// &
'MOD_V3_01_<xx>_X64R.exe:'//NEWLINE// &
'A computer program to perform a variety of graphical visualizations of M'// &
'odel Configurations and/or (in)directly related geographical information'// &
'. The iMOD GUI itself if fully written in Fortran9x and compiled by the '// &
'Intel Visual Fortran Compiler Professional v11.1.054 in conjunction with'// &
' Winteracter 10 (Interactive Software Services Ltd (ISS)).'//NEWLINE// &
NEWLINE// &
'- The MODFLOW computational core (iMODFLOW):'//NEWLINE// &
'iMODFLOW_V3_01_<xx>_X32R.exe and'//NEWLINE// &
'iMODFLOW_V3_01_<xx>_X64R.exe and'//NEWLINE// &
'iMODFLOW_V3_01_<xx>_METASWAP_SVN1031_X64R.exe:'//NEWLINE// &
NEWLINE// &
'iMODFLOW is partly based on the USGS MODFLOW2005 s'// &
'ource code; for iMOD the USGS MODFLOW2005 source code has been expanded '// &
'and extensively modified by Stichting Deltares. '//NEWLINE// &
NEWLINE// &
'The original USGS MODFLOW source code can be downloaded from the USGS we'// &
'bsite http://www.usgs.gov/. The original MODFLOW2005 source code incorpo'// &
'rated in the Deltares-executables is covered by the USGS Software User R'// &
'ights Notice; you should have received a copy of this notice along with '// &
'this program. If not, see http://water.usgs.gov/software/help/notice.'//NEWLINE// &
NEWLINE// &
'One of the two X64-bit iMODFLOW-executables includes'// &
' the MetaSWAP-module SVN version number 1031, part of SIMGRO V7_2_22 as described '// &
'in the SIMGRO-release notes ftp://ftp.wur.nl/simgro/doc/Change_log/ Release_Notes_'// &
'SIMGRO_V7_2_25.pdf. The other X64-bit iMODFLOW-executable does '// &
'not include MetaSWAP. MetaSWAP has been developed by Alterra - Wageningen UR. For more '// &
'info on MetaSWAP, see the iMOD user manual, Annex 1. For more info on Al'// &
'terra – Wageningen UR, see http://www.wageningenur.nl/en/Expertise-Servi'// &
'ces/Research-Institutes/alterra.htm.'//NEWLINE// &
NEWLINE// &
'iMOD user manual'//NEWLINE// &
NEWLINE// &
'A pdf-file of the latest version of the iMOD-user manual, downloadable f'// &
'rom oss.deltares.nl/web/iMOD/user-manual.'//NEWLINE// &
NEWLINE// &
'NETCDF'//NEWLINE// &
NEWLINE// &
'iMOD makes use of functions available in the NetCDF library of www.unida'// &
'ta.ucar.edu. The file ‘NetCDF.dll’ is redistributed together with the iM'// &
'OD-executables. The NetCDF.dll-file can be used under the conditions as '// &
'described in http://www.unidata.ucar.edu/software/netcdf/copyright.html.'// &
NEWLINE// &
NEWLINE// &
'OTHER REQUIREMENTS FOR THE USE OF iMOD'//NEWLINE// &
NEWLINE// &
NEWLINE// &
'HARDWARE PLATFORM & OPERATING SYSTEM '//NEWLINE// &
NEWLINE// &
'iMOD works on IBM-compatible personal computers equipped with at least:'//NEWLINE// &
'1. a Pentium or compatible processor;'//NEWLINE// &
'2. 512 MB internal memory (2045MB recommended);'//NEWLINE// &
'3. 100 MB available on the hard disk (10GB is recommended in case large '// &
'model simulations need to be carried out);'//NEWLINE// &
'4. A graphics adapter with 32 MB video memory and screen resolution of 8'// &
'00-600 (256MB video memory and a screen resolution of 1024x768 is recomm'// &
'end). Moreover, an graphical card that supports OpenGL (OpenGL is a trad'// &
'emark of Silicon Graphics Inc.), such as an ATI Radeon HD or NVIDIA grap'// &
'hical card is necessary to use the 3D rendering.'//NEWLINE// &
NEWLINE// &
'Please note: it is permitted to install the Model System on a different '// &
'Hardware Platform as long as it is a computer similar to the above-menti'// &
'oned computer. The transfer of the Model System to a dissimilar computer'// &
' may endanger the working of the Model System and require adjustments in'// &
' the Configuration. '//NEWLINE// &
NEWLINE// &
'iMOD-GUI is available for a 32- and 64-bit system and runs on the following '// &
'platforms: Windows XP / Server 2003 / Vista Business / Vista Ultimate / Server 2008 / 7'// &
'; the iMODFLOW-executable incl. MetaSWAP is available for 64-bit systems '// &
'only, an iMODFLOW-executable excl. MetaSWAP is available for 32- and 64-bit systems.'//NEWLINE// &
NEWLINE// &
'SYSTEM SOFTWARE'//NEWLINE// &
NEWLINE// &
'Adobe Acrobat is a family of application software developed by Adobe Sys'// &
'tems to view, create, manipulate, print and manage files in Portable Doc'// &
'ument Format (PDF). All members of the family, except Adobe Reader (form'// &
'erly Acrobat Reader), are commercial software; Adobe Reader however, is '// &
'available as freeware and can be downloaded from Adobe‘s web site. Adobe'// &
' Reader enables users to view and print PDF files but has negligible PDF'// &
' creation capabilities. Acrobat and Reader are widely used as a way to p'// &
'resent information with a fixed layout similar to a paper publication.'//NEWLINE// &
NEWLINE// &
NEWLINE// &
'Stichting Deltares'//NEWLINE// &
'Boussinesqweg 1'//NEWLINE// &
'P.O. Box 177'//NEWLINE// &
'2600 MH Delft, The Netherlands'//NEWLINE// &
'Tel:	 +31 (0) 88 335 82 73'//NEWLINE// &
'Fax:	 +31 (0) 88 355 85 82'//NEWLINE// &
'e-mail: info@deltares.nl'//NEWLINE// &
'web:    www.deltares.com'//NEWLINE// &
'Chamber of Commerce no. 41146461'

 IF(ID.LT.0)THEN
  WRITE(ABS(ID),'(A)') TRIM(STR)
 ELSE
  CALL WDIALOGPUTSTRING(ID,TRIM(STR))
 ENDIF
 
 DEALLOCATE(STR)
 
 END SUBROUTINE IMOD_PUTLICENSE
 
 !###====================================================================
 SUBROUTINE IMOD_STARTSCREEN()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I
 REAL :: X

 CALL WDIALOGLOAD(ID_DSTARTSCREEN,ID_DSTARTSCREEN)
 CALL WDIALOGTITLE('iMOD, Interactive Modelling (V'//TRIM(RVERSION)//')')
 CALL IMOD_STARTLICENSE(IDF_LABEL1)
 CALL WDIALOGPUTIMAGE(IDF_PICTURE3,ID_ICONMAIN,1)
 
 CALL WDIALOGSHOW(-1,-1,0,2)
 CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,1,50)
 DO I=1,50
  CALL RANDOM_NUMBER(X)
  CALL IOSWAIT(INT(X*8.0))
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,I,0)
 END DO
 CALL IOSWAIT(150)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE IMOD_STARTSCREEN

END MODULE
