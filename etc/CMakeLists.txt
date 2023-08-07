# (C) Crown Copyright 2021, the Met Office. All rights reserved.
#

file(INSTALL "global" "ukv" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/")

install(
  DIRECTORY "global" "ukv"
  DESTINATION "${CMAKE_INSTALL_SYSCONFDIR}/${PROJECT_NAME}/"
)
