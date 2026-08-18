# (C) Crown Copyright Met Office. All rights reserved.
#
#
# Syntax: CREATE_SYMLINKS(src dst)
#
# Arguments:
# - src: Source folder
# - dst: Destination folder
#
# Creates symbolic links in the destination folder pointing to the specified
# files in the source folder, preserving subfolder hierarchy.
function(CREATE_SYMLINKS src dst)
  file(MAKE_DIRECTORY ${dst})
  foreach(FILENAME ${ARGN})
    get_filename_component(absolute_subdir ${src}/${FILENAME} DIRECTORY)
    file(RELATIVE_PATH relative_subdir ${src} ${absolute_subdir})
    file(MAKE_DIRECTORY ${dst}/${relative_subdir})
    file(CREATE_LINK ${src}/${FILENAME} ${dst}/${FILENAME} SYMBOLIC)
  endforeach()
endfunction()
