set(SOURCE
  utl.f90
  utl.h
  utl1.inc
  utl2.inc
)
if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  set_source_files_properties(utl.f90 PROPERTIES COMPILE_FLAGS -cpp)
endif()

add_library(UTL ${SOURCE})

# see utl.h
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  target_compile_definitions(UTL PUBLIC "IFORT")
endif()
if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  target_compile_definitions(UTL PUBLIC "linux")
endif()

set_property(TARGET UTL
  PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
