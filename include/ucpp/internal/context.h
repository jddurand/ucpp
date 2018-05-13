#ifndef UCPP_INTERNAL_CONTEXT_H
#define UCPP_INTERNAL_CONTEXT_H

#include <ucpp/tune.h>
#include <setjmp.h>

/* order is important */
enum {
	S_START, S_SPACE, S_BANG, S_STRING, S_STRING2, S_COLON,
	S_SHARP, S_PCT, S_PCT2, S_PCT3, S_AMPER, S_CHAR, S_CHAR2, S_STAR,
	S_PLUS, S_MINUS, S_DOT, S_DOT2, S_SLASH, S_NUMBER, S_NUMBER2, S_LT,
	S_LT2, S_EQ, S_GT, S_GT2, S_CIRC, S_PIPE, S_BACKSLASH,
	S_COMMENT, S_COMMENT2, S_COMMENT3, S_COMMENT4, S_COMMENT5,
	S_NAME, S_NAME_BS, S_LCHAR,
	MSTATE,
	S_ILL, S_DDOT, S_DDSHARP, S_BS, S_ROGUE_BS, S_BEHEAD, S_DECAY,
	S_TRUNC, S_TRUNCC, S_OUCH
};

#define CMCR	2

typedef struct machine_state {
	int state;
	unsigned char input[CMCR];
	int new_state;
} machine_state_t;

struct found_file {
	hash_item_header head;    /* first field */
	char *name;
	char *protect;
};

/*
 * For files from system include path.
 */
struct found_file_sys {
	hash_item_header head;    /* first field */
	struct found_file *rff;
	int incdir;
};

struct file_context {
	struct lexer_state ls;
	char *name, *long_name;
	int incdir;
};

struct ucpp_context {
  /*
   * cppm is the table used to store the automaton: if we are in state s
   * and we read character c, we apply the action cppm[s][c] (jumping to
   * another state, or emitting a token).
   * cppm_vch is the table for the special virtual character "end of input"
   */
  int cppm[MSTATE][MAX_CHAR_VAL];
  int cppm_vch[MSTATE];             /* Table for special virtual character "end of input" */
  struct machine_state *cppms;
  /*
   * Assertion support. Each assertion is indexed by its predicate, and
   * the list of 'questions' which yield a true answer.
   */
  HTT assertions;
  int assertions_init_done;
  /*
   * To speed up deeply nested and repeated inclusions, we:
   * -- use a hash table to remember where we found each file
   * -- remember when the file is protected by a #ifndef/#define/#endif
   *    construction; we can then avoid including several times a file
   *    when this is not necessary.
   * -- remember in which directory, in the include path, the file was found.
   */
  HTT found_files;
  HTT found_files_sys;
  int found_files_init_done;
  int found_files_sys_init_done;
  /*
   * we store macros in a hash table, and retrieve them using their name
   * as identifier.
   */
  HTT macros;
  int macros_init_done;
  /*
   * eval stuff
   */
  JMP_BUF eval_exception;
  long eval_line;
  int emit_eval_warnings;
  /*
   * include path
   */
  char **include_path;
  size_t include_path_nb;
  /*
   * Current filename and include dir 
   */
  char *current_filename;
  char *current_long_filename;
  int current_incdir;
  /*
   * #ifndef/#define/#endif protection mechanism
   */
  struct protect protect_detect;
  struct protect *protect_detect_stack;
  /*
   * Find a file by looking through the include path.
   * return value: a FILE * on the file, opened in "r" mode, or 0.
   *
   * find_file_error will contain:
   *   FF_ERROR      on error (file not found or impossible to read)
   *   FF_PROTECT    file is protected and therefore useless to read
   *   FF_KNOWN      file is already known
   *   FF_UNKNOWN    file was not already known
   */
  int find_file_error;
#ifdef UCPP_MMAP
  /*
   * We open() the file, then fdopen() it and fseek() to its end. If the
   * fseek() worked, we try to mmap() the file, up to the point where we
   * arrived.
   * On an architecture where end-of-lines are multibytes and translated
   * into single '\n', bad things could happen. We strongly hope that, if
   * we could fseek() to the end but could not mmap(), then we can get back.
   */
  void *find_file_map;
  size_t map_length;
#endif
  struct file_context *ls_stack;
  size_t ls_depth;
};

#endif /* UCPP_INTERNAL_CONTEXT_H */
