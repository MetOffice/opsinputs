# (C) Crown Copyright 2021, the Met Office. All rights reserved.
#
# Refer to COPYRIGHT.txt of this distribution for details.

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(OPS_DEFAULT_Fortran_FLAGS "\
-fdefault-real-8 \
-fdefault-integer-8 \
-Wall \
-Wtabs \
-Wunderflow \
-Wno-conversion \
-Wno-integer-division \
-Wno-character-truncation \
-fimplicit-none \
${CMAKE_Fortran_FLAGS}"
  )
  set(CMAKE_Fortran_DEBUG_FLAGS "
-fbacktrace \
-fdump-core \
-fbounds-check \
-frange-check \
${CMAKE_Fortran_DEBUG_FLAGS}"
  )
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(OPS_DEFAULT_Fortran_FLAGS "\
-warn all \
-warn errors \
-warn notruncated_source \
-warn nointerfaces \
-implicitnone \
-traceback \
-integer-size 64 \
-real-size 64 \
-nogen-interfaces \
-diag-disable 10121 \
-assume norealloc_lhs \
${CMAKE_Fortran_FLAGS}"
#-Wp,-P \
  )
  set(CMAKE_Fortran_DEBUG_FLAGS "\
-check bounds \
-check format \
-check pointers \ 
-check uninit \
${CMAKE_Fortran_DEBUG_FLAGS}"
  )
endif()

list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )

if( NOT (MPI_FOUND AND MPI_Fortran_FOUND) )
  find_package( MPI REQUIRED COMPONENTS Fortran )
endif()

add_subdirectory(odb)
add_subdirectory(gcom)
add_subdirectory(ops)
