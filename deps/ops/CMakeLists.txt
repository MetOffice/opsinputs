# (C) Crown Copyright 2021, the Met Office. All rights reserved.
#

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  if("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "10.1.0")
    set(OPS_EXTRA_Fortran_FLAGS "-std=f2003 -fallow-argument-mismatch")
  else()
    set(OPS_EXTRA_Fortran_FLAGS "-std=f2003")
  endif()
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(OPS_EXTRA_Fortran_FLAGS "-stand f03")
endif()

set(CMAKE_Fortran_FLAGS "\
${OPS_DEFAULT_Fortran_FLAGS} \
${OPS_EXTRA_Fortran_FLAGS}")

set(OPS_SOURCE_FILES
  code/GenMod_ModelIO/GenMod_ModelIO.F90
  code/OpsMod_AirQuality/OpsMod_AirQuality.f90
  code/OpsMod_Argument/OpsMod_Argument.f90
  code/OpsMod_CXInfo/OpsMod_CXInfo.f90
  code/OpsMod_Control/OpsMod_Control.f90
  code/OpsMod_DateTime/OpsMod_DateTime.f90
  code/OpsMod_GatherSpread/OpsMod_GatherSpread.F90
  code/OpsMod_HorizontalInterp/OpsMod_HorizontalInterp.f90
  code/OpsMod_ModelColumnIO/OpsMod_ModelColumnIO.f90
  code/OpsMod_ModelIO/OpsMod_ModelIO.f90
  code/OpsMod_ModelObInfo/OpsMod_ModelObInfo.f90
  code/OpsMod_ObTypeUtils/OpsMod_ObTypeUtils.f90
  code/OpsMod_ObsInfo/OpsMod_MiscTypes.f90
  code/OpsMod_ObsInfo/OpsMod_ObsInfo.f90
  code/OpsMod_Process/OpsMod_Process.f90
  code/OpsMod_QC/OpsMod_QC.f90
  code/OpsMod_QC/OpsMod_QCBuddy.f90
  code/OpsMod_UMImport/OpsMod_Portio_Wrappers.f90
  code/OpsMod_UMImport/ops_ereport_mod.F90
  code/OpsMod_UMImport/ops_portio2a.c
  code/OpsMod_UMImport/ops_portutils.c
  code/OpsMod_Utilities/OpsMod_Utilities.f90
  code/OpsMod_VarobsLib/OpsMod_VarobsLib.f90
  code/OpsProg_Utils/OpsMod_UtilsProgs.f90
  public/GenMod_Control/GenMod_Control.F90
  public/GenMod_Control/GenMod_Setup.F90
  public/GenMod_Core/GenMod_Core.F90
  public/GenMod_Platform/GenMod_Matmul.F90
  public/GenMod_Platform/GenMod_Platform.F90
  public/GenMod_Platform/Gen_CPUTime.c
  public/GenMod_Platform/Gen_ClockTicks.c
  public/GenMod_Platform/Gen_Memory.c
  public/GenMod_Sleep/GenMod_Sleep.f90
  public/GenMod_Sleep/Gen_SleepWrapper.c
  public/GenMod_Utilities/GenMod_Utilities.f90
  public/OpsMod_CharUtils/OpsMod_CharUtils.f90
  public/OpsMod_EnvUtils/OpsMod_EnvUtils.F90
  public/OpsMod_IOUtils/OpsMod_IOUtils.f90
  public/OpsMod_IOUtils/ops_file_size.c
  public/OpsMod_MPIInterface/OpsMod_Gcom.F90
  public/OpsMod_MPIInterface/OpsMod_MPLInterface.f90
  public/OpsMod_NetCDF/ops_netcdf_wrappers.f90
  public/OpsMod_Random/OpsMod_Random.f90
  public/OpsMod_Sort/OpsMod_Sort.f90
  public/OpsMod_UMInterface/OpsMod_UMInterface.f90
  public/OpsMod_VerticalInterp/OpsMod_VerticalInterp.f90
  stubs/Ops_Constants/GenMod_CLookAdd.f90
  stubs/Ops_Constants/GenMod_Constants.f90
  stubs/Ops_Constants/GenMod_MiscUMScienceConstants.f90
  stubs/Ops_Constants/GenMod_UMHeaderConstants.f90
  stubs/Ops_Constants/OpsMod_Ancil.f90
  stubs/Ops_Constants/OpsMod_CXIndexes.f90
  stubs/Ops_Constants/OpsMod_Constants.f90
  stubs/Ops_Constants/OpsMod_Kinds.F90
  stubs/Ops_Constants/OpsMod_ObsGroupInfo.f90
  stubs/Ops_Constants/OpsMod_ObsTypes.f90
  stubs/Ops_Constants/OpsMod_SatIds.f90
  stubs/Ops_Constants/OpsMod_Stash.f90
  stubs/Ops_Constants/OpsMod_Varfields.f90
  stubs/OpsMod_AODGeneral/OpsMod_AODGeneral.f90
  stubs/OpsMod_GPSRO/OpsMod_GPSRO.f90
  stubs/OpsMod_GPSRO/OpsMod_GPSROInfo.f90
  stubs/OpsMod_GPSRO/OpsMod_GPSROInfo_BA.f90
  stubs/OpsMod_GroundGPS/OpsMod_GroundGPS.f90
  stubs/OpsMod_ODB/OpsMod_ODBKinds.f90
  stubs/OpsMod_ODB/OpsMod_ODBTypes.f90
  stubs/OpsMod_Radar/OpsMod_Radar.f90
  stubs/OpsMod_RadarZ/OpsMod_RadarZ.f90
  stubs/OpsMod_Sonde/OpsMod_SondeDrift.f90
  stubs/OpsMod_Varobs/OpsMod_Varobs.f90
  stubs/Ops_SatRad_Info/OpsMod_SatRad_RTmodel.f90
  stubs/Ops_SatRad_SetUp/OpsMod_SatRad_SetUp.f90
)

# Intel inserts lines in the source during pre-processing that contain the source file path, if the pathname
# is long enough it can take the source line to more than 132 characters and trigger a compile warning.
# If -warn errors is on this creates an invalid compile failure, so suppress this warning for these files to allow
# compilation to proceed.
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set_source_files_properties(public/GenMod_Platform/GenMod_Platform.F90 PROPERTIES COMPILE_FLAGS "-warn noerrors")
  set_source_files_properties(public/GenMod_Core/GenMod_Core.F90 PROPERTIES COMPILE_FLAGS "-warn noerrors")
  set_source_files_properties(public/GenMod_Control/GenMod_Setup.F90 PROPERTIES COMPILE_FLAGS "-warn noerrors")
  set_source_files_properties(public/Ops_Constants/GenMod_MiscUMScienceConstants.f90 PROPERTIES COMPILE_FLAGS "-warn noerrors")
  set_source_files_properties(code/GenMod_ModelIO/GenMod_ModelIO.F90 PROPERTIES COMPILE_FLAGS "-warn noerrors")
  set_source_files_properties(code/OpsMod_GatherSpread/OpsMod_GatherSpread.F90 PROPERTIES COMPILE_FLAGS "-warn noerrors")
  set_source_files_properties(code/OpsProg_Utils/OpsProg_PrintCXFile.F90 PROPERTIES COMPILE_FLAGS "-warn noerrors")
endif()

# Collect the list of source directories (which will be added to the include path).
foreach (file ${OPS_SOURCE_FILES})
  get_filename_component(dir "${file}" DIRECTORY)
  list(APPEND OPS_SOURCE_DIRS "${dir}")
endforeach()
list(REMOVE_DUPLICATES OPS_SOURCE_DIRS)
include_directories( ${OPS_SOURCE_DIRS} )

# Find extra dependencies

find_package(SHUM REQUIRED)


# Setup the parallel version of the ops library

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  ecbuild_add_library(TARGET ops
    SOURCES ${OPS_SOURCE_FILES}
    LINKER_LANGUAGE CXX)
else()
  ecbuild_add_library(TARGET ops
    SOURCES ${OPS_SOURCE_FILES})
endif()

target_compile_definitions(ops PRIVATE 
  EXTERNAL_GCOM
  FORTRAN_VERSION=2003 
  FDBK_OUT
  LITTLE_END
  UNDERSCORE 
  LOWERCASE
  C_LOW_U
  C_LONG_LONG_INT 
  FRL8 
  _FILE_OFFSET_BITS=64 LFS 
  _LARGEFILE_SOURCE
)

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  target_compile_definitions(ops PRIVATE 
    IFORT_ALLOCATABLE_CHARACTER_BUG
  )
endif()

target_link_libraries(ops
  PUBLIC
  gcom
  odb
  ${SHUM_BYTESWAP_LIBRARY}
  ${SHUM_STRING_CONV_LIBRARY}
  ${SHUM_WGDOS_PACKING_LIBRARY}
  NetCDF::NetCDF_Fortran
)

get_target_property(OPS_Fortran_MODULE_DIRECTORY ops Fortran_MODULE_DIRECTORY)

target_include_directories(ops
  PRIVATE ${SHUM_INCLUDE_DIR}
  PRIVATE ${OPS_SOURCE_DIRS}
  INTERFACE $<BUILD_INTERFACE:${OPS_Fortran_MODULE_DIRECTORY}>
)


# Setup the serial version of the ops library, linked to gcom_serial

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  ecbuild_add_library(TARGET ops_serial
    SOURCES ${OPS_SOURCE_FILES}
    LINKER_LANGUAGE CXX)
else()
  ecbuild_add_library(TARGET ops_serial
    SOURCES ${OPS_SOURCE_FILES})
endif()

target_compile_definitions(ops_serial PRIVATE
  EXTERNAL_GCOM
  FORTRAN_VERSION=2003
  FDBK_OUT
  LITTLE_END
  UNDERSCORE
  LOWERCASE
  C_LOW_U
  C_LONG_LONG_INT
  FRL8
  _FILE_OFFSET_BITS=64 LFS
  _LARGEFILE_SOURCE
)

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  target_compile_definitions(ops_serial PRIVATE
    IFORT_ALLOCATABLE_CHARACTER_BUG
  )
endif()

target_link_libraries(ops_serial
  PUBLIC
  gcom_serial
  odb
  ${SHUM_BYTESWAP_LIBRARY}
  ${SHUM_STRING_CONV_LIBRARY}
  ${SHUM_WGDOS_PACKING_LIBRARY}
  NetCDF::NetCDF_Fortran
)

get_target_property(OPS_SERIAL_Fortran_MODULE_DIRECTORY ops_serial Fortran_MODULE_DIRECTORY)
set(OPS_SERIAL_Fortran_MODULE_DIRECTORY ${OPS_SERIAL_Fortran_MODULE_DIRECTORY}/serial)
set_target_properties(ops_serial PROPERTIES
                      Fortran_MODULE_DIRECTORY ${OPS_SERIAL_Fortran_MODULE_DIRECTORY})

target_include_directories(ops_serial
  PRIVATE ${SHUM_INCLUDE_DIR}
  PRIVATE ${OPS_SOURCE_DIRS}
  INTERFACE $<BUILD_INTERFACE:${OPS_SERIAL_Fortran_MODULE_DIRECTORY}>
)


# Setup executable targets

set(CMAKE_EXECUTABLE_SUFFIX .exe)

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  ecbuild_add_executable(TARGET OpsProg_PrintCXFile
    SOURCES
    code/OpsProg_Utils/OpsMod_PrintCX.f90
    code/OpsProg_Utils/OpsProg_PrintCXFile.F90
    LINKER_LANGUAGE CXX)
else()
  ecbuild_add_executable(TARGET OpsProg_PrintCXFile
    SOURCES
    code/OpsProg_Utils/OpsMod_PrintCX.f90
    code/OpsProg_Utils/OpsProg_PrintCXFile.F90)
endif()
target_link_libraries(OpsProg_PrintCXFile
  PRIVATE ops_serial)

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  ecbuild_add_executable(TARGET OpsProg_PrintVarobs
    SOURCES
    code/OpsProg_Utils/OpsMod_PrintVarobs.f90
    code/OpsProg_Utils/OpsProg_PrintVarobs.F90
    LINKER_LANGUAGE CXX)
else()
  ecbuild_add_executable(TARGET OpsProg_PrintVarobs
    SOURCES
    code/OpsProg_Utils/OpsMod_PrintVarobs.f90
    code/OpsProg_Utils/OpsProg_PrintVarobs.F90)
endif()
target_link_libraries(OpsProg_PrintVarobs
  PRIVATE ops_serial)
