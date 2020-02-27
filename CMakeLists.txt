cmake_minimum_required(VERSION 3.9)

# Establish a build type prior to 'project' command
if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE
      STRING "Choose the type of build." FORCE)
endif()
if(NOT CMAKE_CONFIGURATION_TYPES)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

project(iMOD
  LANGUAGES Fortran C
)

# Common compiler build flags
if("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    if(MSVC)  # Windows
      set(Fortran_BUILD_FLAGS
        /traceback
        # /check:all
        /debug:all
        # /fpe:0
      )
    else()
      set(Fortran_BUILD_FLAGS
        -traceback
        # -check all
        -debug all
        # -fpe0
      )
    endif()
  endif()
endif()

# Common compiler warning flags
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
  set(_ifort_diag_disable "8291")
  # remark #8291: Recommended relationship between field width 'W' and ...
  if(MSVC)  # Windows
    set(Fortran_WARN_FLAGS
      /warn:general
      /Qdiag-disable:${_ifort_diag_disable}
    )
  else()  # Linux / macOS
    set(Fortran_WARN_FLAGS
      # -warn general
      -diag-disable ${_ifort_diag_disable}
    )
  endif()
endif()

add_subdirectory(UTL)
add_subdirectory(MODFLOW)
# add_subdirectory(iMOD)
