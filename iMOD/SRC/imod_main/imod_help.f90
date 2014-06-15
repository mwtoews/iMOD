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
      MODULE IMODHELP
      IMPLICIT NONE
      INTEGER, PARAMETER :: IH_INTRODUCTIE                =  1001
      INTEGER, PARAMETER :: IH_DATAVIEW                   =  1002
      INTEGER, PARAMETER :: IH_VOVERLAY                   =  1003
      INTEGER, PARAMETER :: IH_TAGS                       =  1004
      INTEGER, PARAMETER :: IH_PROJECTKAART               =  1005
      INTEGER, PARAMETER :: IH_KAARTBEKIJKEN              =  1006
      INTEGER, PARAMETER :: IH_LEGBEWERKEN                =  1007
      INTEGER, PARAMETER :: IH_PROFILE                    =  1008
      INTEGER, PARAMETER :: IH_PROFILESTART               =  1009
      INTEGER, PARAMETER :: IH_TOOLBOXMODEL               =  1010
      INTEGER, PARAMETER :: IH_TOOLBOXIRDATABASE          =  1011
      INTEGER, PARAMETER :: IH_TOOLBOXWATERBALANS         =  1012
      INTEGER, PARAMETER :: IH_TOOLBOXGXG                 =  1013		!aanpassen is nu 1012
      INTEGER, PARAMETER :: IH_TOOLBOXTIJDREEKS           =  1014
      INTEGER, PARAMETER :: IH_PREFERENCES                =  1015
      INTEGER, PARAMETER :: IH_STARTSCREEN                =  1016
      INTEGER, PARAMETER :: IH_TOOLBOXSCENARIO            =  1017
      INTEGER, PARAMETER :: IH_MATH                       =  1018
      INTEGER, PARAMETER :: IH_MAPEDIT                    =  1019
      INTEGER, PARAMETER :: IH_ADDTOPO                    =  1020
      INTEGER, PARAMETER :: IH_GOTOXY			  =  1021
      INTEGER, PARAMETER :: IH_PATHLINES	          =  1022
      INTEGER, PARAMETER :: IH_STARTPOINTS		  =  1023
      INTEGER, PARAMETER :: IH_MAPEDITIPF                 =  1024
      INTEGER, PARAMETER :: IH_TOOLBOXRO                  =  1025
      INTEGER, PARAMETER :: IH_TOOLBOXIRRO                =  1026
      INTEGER, PARAMETER :: IH_IPF                        =  1027
      INTEGER, PARAMETER :: IH_EXTIPF                     =  1028
      INTEGER, PARAMETER :: IH_CREATEIDF		  =  1029
      INTEGER, PARAMETER :: IH_START3D            	  =  1030
      INTEGER, PARAMETER :: IH_3DSETTINGS                 =  1031
      INTEGER, PARAMETER :: IH_CREATEGEN	          =  1032
      INTEGER, PARAMETER :: IH_CREATEIDF_IPF              =  1033
      INTEGER, PARAMETER :: IH_CREATEIDF_GEN              =  1034
      INTEGER, PARAMETER :: IH_CREATEIDF_IFF              =  1035
      INTEGER, PARAMETER :: IH_ISG                        =  1036
      INTEGER, PARAMETER :: IH_IACTTS                     =  1037
      INTEGER, PARAMETER :: IH_TOOLBOXMEAN                =  1038
      INTEGER, PARAMETER :: IH_MAPINFO                    =  1039
      INTEGER, PARAMETER :: IH_IPFINFO                    =  1040
      INTEGER, PARAMETER :: IH_ISGEDIT                    =  1041
      INTEGER, PARAMETER :: IH_ISGEDIT_ATTRIB             =  1042
      INTEGER, PARAMETER :: IH_ISGEDIT_SEARCH             =  1043
      INTEGER, PARAMETER :: IH_QSMEASURE                  =  1044
      INTEGER, PARAMETER :: IH_QSTARGET 	          =  1045
      INTEGER, PARAMETER :: IH_QSMAIN                     =  1046
      INTEGER, PARAMETER :: IH_LEGCLASS                   =  1047
      INTEGER, PARAMETER :: IH_IPFCONFIG                  =  1048
      INTEGER, PARAMETER :: IH_WHATSNEW                   =  1049
      END MODULE IMODHELP   