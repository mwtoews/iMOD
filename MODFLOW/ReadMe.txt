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


THINGS TO KNOW FIRST:
=====================
- A Visual Studio 2010 solution is included (folder VS2010).
- For now, the code only runs on Windows.
- The main part of the iMOD utilities are located in the IMODUTL folder.
- The main program is called driver.f90, located in the DRIVER folder.
- This controller file calls a component based version of MODFLOW-2005,
  which can be seen as a low-level OpenMI implementation. Source code of
  MODFLOW is located in MF2005.
- In this folder, the file mf2005_comp.f shows the different phases in
  execution and the interface routines in case of a coupled code.
- This distributed code is prepared to couple MODFLOW with MetaSWAP,
  TRANSOL or MOZART. However, this code does not provide codes for MetaSWAP,
  TRANSOL or MOZART. If interested, please contact: imod.support@deltares.nl.
- For using iPEST: you should always pre-compile imod_pest.F90 with
  the IPEST flag. Note that in this release iPEST is not fully tested.
