# Syntax: CREATE_SYMLINK(src dst [rel_path1 [rel_path2 [...]]])
#
# Arguments:
# - src: Source folder
# - dst: Destination folder
# - rel_path1, rel_path2, ...: Files within the source folder, specified in terms of their
#                              their relative path with respect to that folder.
#
# Creates symbolic links in the destination folder pointing to the specified files in the source
# folder, preserving subfolder hierarchy.
function(CREATE_SYMLINKS src dst)
  file(MAKE_DIRECTORY ${dst})
  foreach (FILENAME ${ARGN})
    get_filename_component(absolute_subdir ${src}/${FILENAME} DIRECTORY)
    file(RELATIVE_PATH relative_subdir ${src} ${absolute_subdir})
    file(MAKE_DIRECTORY ${dst}/${relative_subdir})
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink
                    ${src}/${FILENAME}
                    ${dst}/${FILENAME})
  endforeach(FILENAME)
endfunction()
