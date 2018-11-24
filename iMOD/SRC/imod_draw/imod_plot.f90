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
!!
MODULE MOD_MOVE

USE WINTERACTER
USE RESOURCE
USE MOD_DBL
USE MOD_IDFPLOT
USE MODPLOT
USE IMODVAR
USE MOD_MANAGER_UTL, ONLY : MANAGERUPDATE
USE MOD_TOPO, ONLY : TOPO1UPDATEMANAGER 
USE MOD_PROFILE_PAR, ONLY : AREA
USE MOD_UTL, ONLY : ITOS,UTL_STOREZOOMEXTENT
USE MOD_PROFILE_UTL, ONLY : PROFILE_PUTBITMAP
USE MOD_IDF, ONLY : IDFREAD
USE MOD_MDF, ONLY : READMDF,UTL_GETUNITMDF,MDF,MDFDEALLOCATE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_GENPLOT_PAR, ONLY : GEN,MXGEN,ACTLISTGEN
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,RTOS,UTL_PLOT1BITMAP,UTL_PLOT2BITMAP
USE MOD_UDF_UTL, ONLY : UDF_OPEN
USE MOD_ISG_PLOT, ONLY : ISGPLOTMINMAX
USE MOD_TAGS, ONLY : TAGZOOM
USE MOD_3D_SETTINGS, ONLY : IMOD3D_DISPLAY_UPDATE

CONTAINS


END MODULE MOD_MOVE
