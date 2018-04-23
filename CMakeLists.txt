CMAKE_MINIMUM_REQUIRED (VERSION 3.0.0 FATAL_ERROR)
IF (NOT MYPACKAGEBOOTSTRAP_DONE)
  INCLUDE ("3rdparty/github/cmake-utils/cmake/MyPackageBootstrap.cmake")
ENDIF ()
#
# Usually the version here correspong to the NEXT version - it is git tagged
# when we do make marpaESLIFPerlRelease
#
MYPACKAGESTART (ucpp 1 3 2)

###########
# Library #
###########
#
# AMIGA is already set eventually by CMake
#
IF (WIN32 AND NOT CYGWIN)
  SET (MSDOS TRUE)
ENDIF ()

INCLUDE (CheckFunctionExists)
CHECK_FUNCTION_EXISTS(strftime HAVE_STRFTIME)
IF (NOT HAVE_STRFTIME)
  SET (NOSTRFTIME TRUE)
ENDIF ()
CHECK_FUNCTION_EXISTS(setbuf HAVE_SETBUF)
IF (NOT HAVE_SETBUF)
  SET (NO_LIBC_BUF TRUE)
ENDIF ()
#
# Keep ucpp native buffering
#
SET (NO_UCPP_BUF FALSE)
#
# Do not use mmap considered beta quality by the author
#
SET (UCPP_MMAP FALSE)
#
# Tokenize pragma
#
SET (PRAGMA_TOKENIZE TRUE)
#
# PRAGMA dump in non-lexer mode
#
SET (PRAGMA_DUMP TRUE)
#
# Disable _Pragma() in some directives
#
SET (NO_PRAGMA_IN_DIRECTIVE FALSE)
#
# Extra space with ##
#
SET (DSHARP_TOKEN_MERGE FALSE)
#
# Count macro replacements
#
SET (INMACRO_FLAG TRUE)
#
# STD include path
#
SET (STD_INCLUDE_PATH 0)
#
# For arithmetic, we search which type is 64 bits long.
#
# First let's see if we have CHAR_BIT
#
INCLUDE (CheckIncludeFile)
CHECK_INCLUDE_FILE ("limits.h" HAVE_LIMITS_H)
IF (HAVE_LIMITS_H)
  INCLUDE (CheckSymbolExists)
  CHECK_SYMBOL_EXISTS(CHAR_BIT "limits.h" HAVE_CHAR_BIT)
  IF (NOT HAVE_CHAR_BIT)
    MESSAGE (STATUS "No CHAR_BIT in limits.h: assuming 8")
    SET (CHAR_BIT_VALUE 8)
  ELSE ()
    SET (CHAR_BIT_VALUE "CHAR_BIT")
  ENDIF ()
ELSE ()
  MESSAGE (STATUS "No limits.h: assuming CHAR_BIT is 8")
  SET (CHAR_BIT_VALUE 8)
ENDIF ()
#
# Arithmetic checks ?
#
SET (ARITHMETIC_CHECKS TRUE)
#
# Std include path ?
#
SET (STD_INCLUDE_PATH "(0)")
#
# Std assert ?
#
SET (STD_ASSERT "(0)")
#
# Std hardcoded macros ?
#
SET (STD_MACROS "(0)")
#
# POSIX jmp ?
#
SET (POSIX_JMP FALSE)
#
# Maximum value plus one of a character handled by the lexer
#
SET (MAX_CHAR_VAL 128)
#
# Unbreakable space ?
#
SET (UNBREAKABLE_SPACE FALSE)
#
# Whitespace tokens ?
#
SET (SEMPER_FIDELIS FALSE)

#
# Source files
# Take care: arith.c is included, should never be compiled standalone
#
SET (ucpp_src
  ${CMAKE_CURRENT_SOURCE_DIR}/src/mem.c
  ${CMAKE_CURRENT_SOURCE_DIR}/src/nhash.c
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp.c
  ${CMAKE_CURRENT_SOURCE_DIR}/src/lexer.c
  ${CMAKE_CURRENT_SOURCE_DIR}/src/assert.c
  ${CMAKE_CURRENT_SOURCE_DIR}/src/macro.c
  ${CMAKE_CURRENT_SOURCE_DIR}/src/eval.c)

MYPACKAGELIBRARY(
  ${CMAKE_CURRENT_SOURCE_DIR}/config.h.in
  ${INCLUDE_OUTPUT_PATH}/ucpp/config.h
  ${ucpp_src})
ADD_DEFINITIONS("-DUCPP_CONFIG")

###############
# Executables #
###############
MYPACKAGEEXECUTABLE(cpp src/cpp.c)
FOREACH (_target cpp cpp_static)
  TARGET_COMPILE_OPTIONS(${_target} PUBLIC "-DSTAND_ALONE")
  SET_TARGET_PROPERTIES(${_target} PROPERTIES OUTPUT_NAME "u${_target}")
ENDFOREACH ()
MYPACKAGETESTEXECUTABLE(atest_native test/atest.c)
TARGET_COMPILE_OPTIONS(atest_native PUBLIC "-DTEST_NATIVE")
MYPACKAGETESTEXECUTABLE(atest_simul test/atest.c)
TARGET_COMPILE_OPTIONS(atest_simul PUBLIC "-DTEST_SIMUL")
MYPACKAGETESTEXECUTABLE(sample test/sample.c)

#########
# Tests #
#########
MYPACKAGECHECK(atest_native)
MYPACKAGECHECK(atest_simul)
MYPACKAGECHECK(sample)

###########
# Install #
###########
# MYPACKAGEMAN("doc/Tutorial/Calculator/README.pod" "MARPAESLIF_CALCULATOR" "3" "${MARPAESLIF_VERSION}")

#############
# Packaging #
#############
MYPACKAGEPACK ("" "UCPP Preprocessor")

#########
# Setup #
#########
MYPACKAGEPRINTSETUP()
