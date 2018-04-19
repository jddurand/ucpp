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

FILE (GLOB ucpp_src ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

MYPACKAGELIBRARY(
  ${CMAKE_CURRENT_SOURCE_DIR}/config.h.in
  ${INCLUDE_OUTPUT_PATH}/config.h
  ${ucpp_src})
ADD_DEFINITIONS("-DUCPP_CONFIG")

###############
# Executables #
###############
# MYPACKAGETESTEXECUTABLE(marpaESLIFTester           test/marpaESLIFTester.c)

################
# Dependencies #
################
# MYPACKAGEDEPENDENCY (genericLogger "${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/github/genericLogger" LIBS)

#########
# Tests #
#########
# MYPACKAGECHECK(marpaESLIFTester)

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
