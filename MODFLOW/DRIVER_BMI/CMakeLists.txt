set(SOURCE
  driver.F90
  driver_bmi.F90
  driver_bmi_module.f90
  driver_module.f90
)

add_library(DRIVER_BMI ${SOURCE})

target_link_libraries(DRIVER_BMI
  PUBLIC
    iMODUTL
	UTL
)

target_include_directories(DRIVER_BMI
  PUBLIC
    $<TARGET_PROPERTY:iMODUTL,Fortran_MODULE_DIRECTORY>
    $<TARGET_PROPERTY:UTL,Fortran_MODULE_DIRECTORY>
)
