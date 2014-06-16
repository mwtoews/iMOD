c   Copyright (C) Stichting Deltares, 2005-2014.
c
c   This file is part of iMOD.
c
c   This program is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, either version 3 of the License, or
c   (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with this program.  If not, see <http://www.gnu.org/licenses/>.
c
c   Contact: imod.support@deltares.nl
c   Stichting Deltares
c   P.O. Box 177
c   2600 MH Delft, The Netherlands.


SOFTWARE WE USED TO COMPILE IMOD:
=================================

- Visual Studio 2008 Professional Edition, version 9.0.30729.1 SP
    (C) 2007 Microsoft Corporation
- Intel (R) Visual Fortran Intel(R) Compiler Professional
    for applications running on Intel(R) 64, Version 11.1.065

NECESSARY SOURCE CODE AND LIBRARIES TO COMPILE IMOD:
====================================================

- The repository ontains the following source:

   \BIN - should contain your binary after compiling the source;

   \BMP - contains bitmaps for iMOD;

   \CUR - contains cursors for iMOD;

   \ICO - contains icons for iMOD;

   \SRC - contains the fortran source code, subdivided in different folders;

   \VS2008 - contains two Solution files for Visual Studio 2008 to compile iMOD.
       (IMPORTANT!: the sln file has been saved with relative pathnames).
     - iMOD.SLN
       compiles iMOD using the WINTERACTER, DELFTIO and NETCDF libraries, resulting in
       support in this iMOD binary for reading SOBEK files and NetCDF files.
     - iMOD_LIGHT.SLN
       compiles iMOD using the WINTERACTER library only, resulting in NO
       support in this iMOD binary for reading SOBEK files and NetCDF files.

   \LIB - should contain libraries for third party software
          (not included in this repository)

     \WINTERACTER - should contain the Winteracter 10.0 or higher library
         (www.winteracter.com)

        \OS_X32 and ..\OS_X64 - depending on the exe that is needed 
          (X32 or X64) put the following files:

          - winteracter.mod
          - winter.lib
          - winttypes.mod

          (add the following files from the c:\Windows\System32 folder)

          - advapi32.lib
          - comdlg32.lib
          - gdi32.lib
          - gui32.lib
          - htmlhelp.lib
          - opengl32.lib
          - shell32.lib
          - user32.lib
          - winmm.lib
          - winspool.lib
          - opengl.mod
          - opengl2.mod

          \include
            - editor.rc
            - colour.rc
            - datesel.rc
            - dirsel.rc
            - grid.rc
            - grid2.rc     
            - hardcopy.rc
            - marksel.rc
            - pagesetup.rc
            - winparam.h
            - iss_next.ico
            - iss_prev.ico
            - iss_editor_16.bmp
            - iss_editor_256.bmp
            - iss_editor2_16.bmp
            - iss_editor2_256.bmp
            - iss_grid_16.bmp
            - iss_grid_256.bmp

     \DELFTIO - should containt the Deltares IO library, needed to read
                in SOBEK files:

        \OS_X32 and  ..\OS_X64 - depending on the exe that is needed
          (X32 or X64) put the following files:

          - delftio.lib
          - dio_2dfield_rw.mod
          - dio_3d_block.mod
          - dio_const_rw.mod
          - dio_ds.mod
          - dio_ds_config.mod
          - dio_ini.mod
          - dio_plt_rw.mod
          - dio_prop.mod
          - dio_shm.mod
          - dio_streams.mod
          - open_mi_dio.mod
     \NETCDF - should contain the NetCDF 3.0 library 

        \OS_X32 and ..\OS_X64 - depending on the exe that is needed 
          (X32 or X64) put the following files:

          \DEBUG and ..\RELEASE

            - netcdf.dll  (IMPORTANT! Supply this file with the the iMOD binary)
            - f77_netcdf.lib
            - f90_netcdf.lib
            - netcdf.lib
            - netcdf.mod

  \TUTORIALS - contains several tutorial data sets as describe on the iMOD Manual:

     \TUT1_MAP_DISPLAY
     \TUT2_DATA_MAP_OPER
     \TUT3_MAP_ANALYSE
     \TUT4_INITIAL_MODELING
     \TUT5_SOLID_BUILDING
     \TUT6_MODEL_SIMULATION
     \TUT7_SCENARIO_TOOL

  readme.txt - this file