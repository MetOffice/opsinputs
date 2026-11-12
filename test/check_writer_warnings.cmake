execute_process(
  COMMAND "${WRITER_EXECUTABLE}" "${WRITER_INPUT}"
  RESULT_VARIABLE result
  OUTPUT_VARIABLE output
  ERROR_VARIABLE errors )
message("${output}${errors}")
if(NOT "${result}" STREQUAL "0")
  message(FATAL_ERROR "Writer test failed: ${result}")
endif()
set( log "${output}${errors}" )
if(log MATCHES "[Vv]ar[Ff]ield (code )?not recogni[sz]ed[: ]+-32768|Stash code not recognized: -32768")
  message(FATAL_ERROR "Writer reported missing-data padding as an unknown field")
endif()
foreach(warning IN LISTS EXPECTED_WARNINGS)
  if(NOT log MATCHES "${warning}")
    message(FATAL_ERROR "Missing expected writer warning: ${warning}")
  endif()
endforeach()
