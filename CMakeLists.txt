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
# For arithmetic, we always use the architecture defined version
# of int. This is why I added the #include <limits.h> in config.h.in
#
# It appears that documentation does not say that SIMUL_NUMBITS should
# always be defined
#
IF (HAVE_SIZEOF_INT)
  SET (SIMUL_NUMBITS ${SIZEOF_INT})
ELSE ()
  SET (SIMUL_NUMBITS sizeof(int))
ENDIF ()

# Arithmetic checks ?
#
SET (ARITHMETIC_CHECKS TRUE)
#
# Std assert ?
#
SET (STD_ASSERT 0)
#
# Std hardcoded macros ?
#
SET (STD_MACROS 0)
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
  ${CMAKE_CURRENT_SOURCE_DIR}/mem.c
  ${CMAKE_CURRENT_SOURCE_DIR}/nhash.c
  ${CMAKE_CURRENT_SOURCE_DIR}/cpp.c
  ${CMAKE_CURRENT_SOURCE_DIR}/lexer.c
  ${CMAKE_CURRENT_SOURCE_DIR}/assert.c
  ${CMAKE_CURRENT_SOURCE_DIR}/macro.c
  ${CMAKE_CURRENT_SOURCE_DIR}/eval.c)

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
