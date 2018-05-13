MACRO (MYPACKAGETESTEXECUTABLE name)
  #
  # Policies - an include is resetting them, so we have to
  # be explicit. We are doing ADD_EXECUTABLE here.
  #
  FOREACH (_policy CMP0063 CMP0018)
    IF (POLICY ${_policy})
      IF (MYPACKAGE_DEBUG)
        MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Setting policy ${_policy} to NEW")
      ENDIF ()
      CMAKE_POLICY (SET ${_policy} NEW)
    ENDIF ()
  ENDFOREACH ()

  IF (MYPACKAGE_DEBUG)
    FOREACH (_source ${ARGN})
      MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Source: ${_source}")
    ENDFOREACH ()
  ENDIF ()

  FOREACH (_name ${name} ${name}_static)
    LIST (APPEND ${PROJECT_NAME}_TEST_EXECUTABLE ${_name})
    IF (MYPACKAGE_DEBUG)
      MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Adding ${_name}")
    ENDIF ()
    ADD_EXECUTABLE (${_name} EXCLUDE_FROM_ALL ${ARGN})
    IF (MYPACKAGE_DEBUG)
      MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Set runtime output directory of ${_name} to ${LIBRARY_OUTPUT_PATH}")
    ENDIF ()
    SET_TARGET_PROPERTIES (${_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${LIBRARY_OUTPUT_PATH})

    IF (${_name} STREQUAL ${name})
      IF (TARGET ${PROJECT_NAME})
        IF (MYPACKAGE_DEBUG)
          MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Adding ${PROJECT_NAME} link library to ${_name}")
        ENDIF ()
        TARGET_LINK_LIBRARIES(${_name} PUBLIC ${PROJECT_NAME})
      ELSE ()
        #
        # Current project does not define a library
        #
        FOREACH (_include_directory output/include include)
          IF (MYPACKAGE_DEBUG)
            MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Adding ${_include_directory} include dependency to ${_name}")
          ENDIF ()
          GET_FILENAME_COMPONENT(_absolute ${_include_directory} ABSOLUTE)
          SET_TARGET_PROPERTIES (${_name} PROPERTIES INCLUDE_DIRECTORIES ${_absolute})
        ENDFOREACH ()
      ENDIF ()
    ENDIF ()

    IF (${_name} STREQUAL ${name}_static)
      IF (MYPACKAGE_DEBUG)
        MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Setting -D${PROJECT_NAME}_STATIC to ${_name}")
      ENDIF ()
      TARGET_COMPILE_DEFINITIONS(${_name} PRIVATE -D${PROJECT_NAME}_STATIC)
      IF (TARGET ${PROJECT_NAME}_static)
        IF (MYPACKAGE_DEBUG)
          MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Adding ${PROJECT_NAME}_static link library to ${_name}")
        ENDIF ()
        TARGET_LINK_LIBRARIES(${_name} PUBLIC ${PROJECT_NAME}_static)
      ELSE ()
        #
        # Current project does not define a static library
        #
        FOREACH (_include_directory output/include include)
          IF (MYPACKAGE_DEBUG)
            MESSAGE (STATUS "[${PROJECT_NAME}-TESTEXECUTABLE-DEBUG] Adding ${_include_directory} include dependency to ${_name}")
          ENDIF ()
          GET_FILENAME_COMPONENT(_absolute ${_include_directory} ABSOLUTE)
          SET_TARGET_PROPERTIES (${_name} PROPERTIES INCLUDE_DIRECTORIES ${_absolute})
        ENDFOREACH ()
      ENDIF ()
    ENDIF ()
  ENDFOREACH ()

ENDMACRO()
