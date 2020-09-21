# (C) Crown Copyright 2020, the Met Office. All rights reserved.
#
# Refer to COPYRIGHT.txt of this distribution for details.

if( NOT CMAKE_BUILD_TYPE MATCHES "Debug" )
  add_definitions( -DNDEBUG )
endif( )
