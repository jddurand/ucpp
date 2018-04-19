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
# Amiga ?
#
IF (WIN32 AND NOT CYGWIN)
  SET (UUCP_MSDOS TRUE)
ENDIF ()

INCLUDE (CheckFunctionExists)
CHECK_FUNCTION_EXISTS(strftime HAVE_STRFTIME)
IF (NOT HAVE_STRFTIME)
  SET (UUCP_NOSTRFTIME TRUE)
ENDIF ()

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
MYPACKAGEPACK ("" "UUCP Preprocessor")

#########
# Setup #
#########
MYPACKAGEPRINTSETUP()
