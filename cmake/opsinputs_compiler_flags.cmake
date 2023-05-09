# (C) Crown Copyright 2020, the Met Office. All rights reserved.
#

if( NOT CMAKE_BUILD_TYPE MATCHES "Debug" )
  add_definitions( -DNDEBUG )
endif( )
