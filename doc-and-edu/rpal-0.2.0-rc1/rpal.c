/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     T_LET = 258,
     T_IN = 259,
     T_FN = 260,
     T_WHERE = 261,
     T_AUG = 262,
     T_OR = 263,
     T_NOT = 264,
     T_GR = 265,
     T_GE = 266,
     T_LS = 267,
     T_LE = 268,
     T_EQ = 269,
     T_NE = 270,
     T_TRUE = 271,
     T_FALSE = 272,
     T_NIL = 273,
     T_DUMMY = 274,
     T_WITHIN = 275,
     T_AND = 276,
     T_REC = 277,
     T_POW = 278,
     T_ARROW = 279,
     T_IDENTIFIER = 280,
     T_STRING = 281,
     T_INTEGER = 282,
     T_OPERATOR = 283
   };
#endif
/* Tokens.  */
#define T_LET 258
#define T_IN 259
#define T_FN 260
#define T_WHERE 261
#define T_AUG 262
#define T_OR 263
#define T_NOT 264
#define T_GR 265
#define T_GE 266
#define T_LS 267
#define T_LE 268
#define T_EQ 269
#define T_NE 270
#define T_TRUE 271
#define T_FALSE 272
#define T_NIL 273
#define T_DUMMY 274
#define T_WITHIN 275
#define T_AND 276
#define T_REC 277
#define T_POW 278
#define T_ARROW 279
#define T_IDENTIFIER 280
#define T_STRING 281
#define T_INTEGER 282
#define T_OPERATOR 283




/* Copy the first part of user declarations.  */
#line 19 "rpal.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#if HAVE_GETOPT_H && HAVE_GETOPT_LONG
#include <getopt.h>
#else
#include "lib/getopt.h"
#endif
#include <libguile.h>
#ifdef HAVE_READLINE_READLINE_H
#include <readline/readline.h>
#include <readline/history.h>
#endif

  SCM astree;
  void yyrestart(FILE*);
  void yyerror(const char*);
  int yylex();

#ifdef DEPRECATED_EVAL
#define scm_eval(a,b) scm_eval2(a,b)
#endif

#ifdef HAVE_LIBREADLINE
#define FREE_LINE(str) free(str)
#else
#define FREE_LINE(str)
#endif


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 51 "rpal.y"
{
  SCM scmval;
}
/* Line 193 of yacc.c.  */
#line 188 "rpal.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 201 "rpal.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  49
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   153

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  34
/* YYNRULES -- Number of rules.  */
#define YYNRULES  77
/* YYNRULES -- Number of states.  */
#define YYNSTATES  131

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   283

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    32,     2,
      38,    39,    35,    33,    30,    34,    29,    36,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    40,     2,     2,    37,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    31,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,    10,    15,    17,    21,    23,    26,
      28,    32,    36,    37,    41,    43,    49,    51,    55,    57,
      61,    63,    66,    68,    72,    76,    80,    84,    88,    92,
      94,    98,   102,   105,   108,   110,   114,   118,   120,   124,
     126,   131,   133,   136,   138,   140,   142,   144,   146,   148,
     150,   154,   156,   160,   162,   165,   167,   171,   175,   176,
     179,   181,   185,   190,   194,   196,   200,   203,   206,   208,
     212,   216,   217,   220,   223,   224,   226,   228
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      42,     0,    -1,    43,    -1,     3,    60,     4,    43,    -1,
       5,    70,    29,    43,    -1,    44,    -1,    45,     6,    64,
      -1,    45,    -1,    48,    46,    -1,    48,    -1,    30,    48,
      47,    -1,    30,    48,    47,    -1,    -1,    48,     7,    49,
      -1,    49,    -1,    50,    24,    49,    31,    49,    -1,    50,
      -1,    50,     8,    51,    -1,    51,    -1,    51,    32,    52,
      -1,    52,    -1,     9,    53,    -1,    53,    -1,    54,    10,
      54,    -1,    54,    11,    54,    -1,    54,    12,    54,    -1,
      54,    13,    54,    -1,    54,    14,    54,    -1,    54,    15,
      54,    -1,    54,    -1,    54,    33,    55,    -1,    54,    34,
      55,    -1,    33,    55,    -1,    34,    55,    -1,    55,    -1,
      55,    35,    56,    -1,    55,    36,    56,    -1,    56,    -1,
      57,    23,    56,    -1,    57,    -1,    57,    37,    72,    58,
      -1,    58,    -1,    58,    59,    -1,    59,    -1,    72,    -1,
      74,    -1,    73,    -1,    16,    -1,    17,    -1,    18,    -1,
      38,    43,    39,    -1,    19,    -1,    61,    20,    60,    -1,
      61,    -1,    64,    62,    -1,    64,    -1,    21,    64,    63,
      -1,    21,    64,    63,    -1,    -1,    22,    65,    -1,    65,
      -1,    67,    40,    43,    -1,    72,    70,    40,    43,    -1,
      38,    60,    39,    -1,    72,    -1,    38,    67,    39,    -1,
      38,    39,    -1,    72,    68,    -1,    72,    -1,    30,    72,
      69,    -1,    30,    72,    69,    -1,    -1,    66,    71,    -1,
      66,    71,    -1,    -1,    25,    -1,    26,    -1,    27,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    66,    66,    69,    70,    71,    74,    76,    79,    82,
      85,    88,    89,    92,    95,    98,   101,   104,   106,   109,
     111,   114,   115,   118,   119,   120,   121,   122,   123,   124,
     127,   128,   129,   130,   131,   134,   135,   136,   139,   140,
     143,   146,   149,   150,   153,   154,   155,   156,   157,   158,
     159,   160,   163,   164,   167,   168,   171,   174,   175,   178,
     179,   182,   183,   186,   189,   190,   191,   194,   196,   199,
     202,   203,   206,   209,   210,   213,   216,   219
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "T_LET", "T_IN", "T_FN", "T_WHERE",
  "T_AUG", "T_OR", "T_NOT", "T_GR", "T_GE", "T_LS", "T_LE", "T_EQ", "T_NE",
  "T_TRUE", "T_FALSE", "T_NIL", "T_DUMMY", "T_WITHIN", "T_AND", "T_REC",
  "T_POW", "T_ARROW", "T_IDENTIFIER", "T_STRING", "T_INTEGER",
  "T_OPERATOR", "'.'", "','", "'|'", "'&'", "'+'", "'-'", "'*'", "'/'",
  "'@'", "'('", "')'", "'='", "$accept", "Program", "Expr", "ExprWhere",
  "Tuple", "TupleRest", "TupleRest2", "AugmentedTuple", "BasicTuple",
  "Boolean", "BooleanT", "BooleanS", "BooleanP", "Arith", "ArithT",
  "ArithF", "ArithP", "Rator", "Rand", "Def", "DefA", "DefARest",
  "DefARest2", "DefRec", "DefB", "VarBasic", "VarList", "VarListRest",
  "VarListRest2", "VarPlus", "VarPlusRest", "Identifier", "String",
  "Integer", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,    46,
      44,   124,    38,    43,    45,    42,    47,    64,    40,    41,
      61
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    41,    42,    43,    43,    43,    44,    44,    45,    45,
      46,    47,    47,    48,    48,    49,    49,    50,    50,    51,
      51,    52,    52,    53,    53,    53,    53,    53,    53,    53,
      54,    54,    54,    54,    54,    55,    55,    55,    56,    56,
      57,    57,    58,    58,    59,    59,    59,    59,    59,    59,
      59,    59,    60,    60,    61,    61,    62,    63,    63,    64,
      64,    65,    65,    65,    66,    66,    66,    67,    67,    68,
      69,    69,    70,    71,    71,    72,    73,    74
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     4,     4,     1,     3,     1,     2,     1,
       3,     3,     0,     3,     1,     5,     1,     3,     1,     3,
       1,     2,     1,     3,     3,     3,     3,     3,     3,     1,
       3,     3,     2,     2,     1,     3,     3,     1,     3,     1,
       4,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       3,     1,     3,     1,     2,     1,     3,     3,     0,     2,
       1,     3,     4,     3,     1,     3,     2,     2,     1,     3,
       3,     0,     2,     2,     0,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,    47,    48,    49,    51,    75,    76,
      77,     0,     0,     0,     0,     2,     5,     7,     9,    14,
      16,    18,    20,    22,    29,    34,    37,    39,    41,    43,
      44,    46,    45,     0,     0,     0,    53,    55,    60,     0,
      68,     0,    74,     0,    64,    21,    32,    33,     0,     1,
       0,     0,     0,     8,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    42,
      59,     0,     0,     0,     0,    54,     0,     0,    67,     0,
      66,     0,    68,    74,    72,     0,    50,     6,    13,    12,
      17,     0,    19,    23,    24,    25,    26,    27,    28,    30,
      31,    35,    36,    38,     0,    63,     3,    52,    58,    61,
      71,     0,    65,    73,     4,     0,    10,     0,    40,     0,
      56,     0,    69,    62,    12,    15,    58,    71,    11,    57,
      70
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    14,    15,    16,    17,    53,   116,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    35,
      36,    75,   120,    37,    38,    42,    39,    78,   122,    43,
      84,    30,    31,    32
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -48
static const yytype_int8 yypact[] =
{
      78,   -15,   -16,   105,   -48,   -48,   -48,   -48,   -48,   -48,
     -48,   109,   109,    78,    16,   -48,   -48,    18,     8,   -48,
      21,    22,   -48,   -48,    45,    33,   -48,    -6,   109,   -48,
     -48,   -48,   -48,    28,   -15,    44,    30,    50,   -48,    40,
      60,    12,   -16,    55,   -48,   -48,    33,    33,    23,   -48,
     -15,     9,     9,   -48,     9,     9,     9,   105,   105,   105,
     105,   105,   105,   109,   109,   109,   109,   109,    61,   -48,
     -48,    49,    78,   -15,   -15,   -48,    78,    61,   -48,    51,
     -48,    53,    59,   -16,   -48,    78,   -48,   -48,   -48,    14,
      22,    68,   -48,   -14,   -14,   -14,   -14,   -14,   -14,    33,
      33,   -48,   -48,   -48,   109,   -48,   -48,   -48,    80,   -48,
      72,    78,   -48,   -48,   -48,     9,   -48,     9,   109,   -15,
     -48,    61,   -48,   -48,    14,   -48,    80,    72,   -48,   -48,
     -48
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -48,   -48,   -11,   -48,   -48,   -48,   -10,   -38,   -47,   -48,
      56,    57,   106,    91,     0,    41,   -48,    11,   -25,   -21,
     -48,   -48,    -9,   -44,    86,   -37,    88,   -48,     6,    97,
      58,    -1,   -48,   -48
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      40,    44,    48,    69,    88,    83,    87,    33,    91,     8,
       8,    46,    47,    71,    89,    51,    49,    67,     3,    63,
      64,    51,    41,    34,    50,     4,     5,     6,     7,    54,
     108,    68,    40,    40,     8,     9,    10,     8,    52,    44,
      82,    44,    11,    12,   115,    55,    83,    13,    72,    40,
      73,    80,   107,     8,    56,    57,    58,    59,    60,    61,
      62,   106,    86,    99,   100,   109,    34,   104,    65,    66,
     125,    74,    40,    40,   114,   126,   110,   124,    63,    64,
      76,     1,    44,     2,    85,     8,     8,     3,   105,    77,
      77,   111,   112,    69,     4,     5,     6,     7,    41,   117,
     123,   119,   121,     8,     9,    10,   101,   102,   103,    45,
      90,    11,    12,    92,   128,   118,    13,   129,    40,    70,
     127,     4,     5,     6,     7,     4,     5,     6,     7,    81,
       8,     9,    10,   130,     8,     9,    10,    79,    11,    12,
       0,   113,     0,    13,     0,     0,     0,    13,    93,    94,
      95,    96,    97,    98
};

static const yytype_int8 yycheck[] =
{
       1,     2,    13,    28,    51,    42,    50,    22,    55,    25,
      25,    11,    12,    34,    52,     7,     0,    23,     9,    33,
      34,     7,    38,    38,     6,    16,    17,    18,    19,     8,
      74,    37,    33,    34,    25,    26,    27,    25,    30,    40,
      41,    42,    33,    34,    30,    24,    83,    38,     4,    50,
      20,    39,    73,    25,    32,    10,    11,    12,    13,    14,
      15,    72,    39,    63,    64,    76,    38,    68,    35,    36,
     117,    21,    73,    74,    85,   119,    77,   115,    33,    34,
      40,     3,    83,     5,    29,    25,    25,     9,    39,    30,
      30,    40,    39,   118,    16,    17,    18,    19,    38,    31,
     111,    21,    30,    25,    26,    27,    65,    66,    67,     3,
      54,    33,    34,    56,   124,   104,    38,   126,   119,    33,
     121,    16,    17,    18,    19,    16,    17,    18,    19,    41,
      25,    26,    27,   127,    25,    26,    27,    40,    33,    34,
      -1,    83,    -1,    38,    -1,    -1,    -1,    38,    57,    58,
      59,    60,    61,    62
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     5,     9,    16,    17,    18,    19,    25,    26,
      27,    33,    34,    38,    42,    43,    44,    45,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      72,    73,    74,    22,    38,    60,    61,    64,    65,    67,
      72,    38,    66,    70,    72,    53,    55,    55,    43,     0,
       6,     7,    30,    46,     8,    24,    32,    10,    11,    12,
      13,    14,    15,    33,    34,    35,    36,    23,    37,    59,
      65,    60,     4,    20,    21,    62,    40,    30,    68,    70,
      39,    67,    72,    66,    71,    29,    39,    64,    49,    48,
      51,    49,    52,    54,    54,    54,    54,    54,    54,    55,
      55,    56,    56,    56,    72,    39,    43,    60,    64,    43,
      72,    40,    39,    71,    43,    30,    47,    31,    58,    21,
      63,    30,    69,    43,    48,    49,    64,    72,    47,    63,
      69
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 66 "rpal.y"
    { astree = (yyvsp[(1) - (1)].scmval); }
    break;

  case 3:
#line 69 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("let"), (yyvsp[(2) - (4)].scmval), (yyvsp[(4) - (4)].scmval)); }
    break;

  case 4:
#line 70 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("lambda"), (yyvsp[(2) - (4)].scmval), (yyvsp[(4) - (4)].scmval)); }
    break;

  case 5:
#line 71 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 6:
#line 74 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("where"),
						  (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 7:
#line 76 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 8:
#line 79 "rpal.y"
    { 
  (yyval.scmval) = scm_append(scm_list_2(scm_list_2(scm_str2symbol("tau"), (yyvsp[(1) - (2)].scmval)), (yyvsp[(2) - (2)].scmval)));
}
    break;

  case 9:
#line 82 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 10:
#line 85 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(2) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 11:
#line 88 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(2) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 12:
#line 89 "rpal.y"
    { (yyval.scmval) = SCM_EOL; }
    break;

  case 13:
#line 92 "rpal.y"
    { 
  (yyval.scmval) = scm_list_3(scm_str2symbol("aug"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval));
}
    break;

  case 14:
#line 95 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 15:
#line 98 "rpal.y"
    { 
  (yyval.scmval) = scm_list_4(scm_str2symbol("ternary"), (yyvsp[(1) - (5)].scmval), (yyvsp[(3) - (5)].scmval), (yyvsp[(5) - (5)].scmval));
}
    break;

  case 16:
#line 101 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 17:
#line 104 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("or"), 
						 (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 18:
#line 106 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 19:
#line 109 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("&"), 
						  (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 20:
#line 111 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 21:
#line 114 "rpal.y"
    { (yyval.scmval) = scm_list_2(scm_str2symbol("not"), (yyvsp[(2) - (2)].scmval)); }
    break;

  case 22:
#line 115 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 23:
#line 118 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("gr"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 24:
#line 119 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("ge"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 25:
#line 120 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("ls"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 26:
#line 121 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("le"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 27:
#line 122 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("eq"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 28:
#line 123 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("ne"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 29:
#line 124 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 30:
#line 127 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("+"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 31:
#line 128 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("-"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 32:
#line 129 "rpal.y"
    { (yyval.scmval) = (yyvsp[(2) - (2)].scmval); }
    break;

  case 33:
#line 130 "rpal.y"
    { (yyval.scmval) = scm_list_2(scm_str2symbol("neg"), (yyvsp[(2) - (2)].scmval)); }
    break;

  case 34:
#line 131 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 35:
#line 134 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("*"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 36:
#line 135 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("/"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 37:
#line 136 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 38:
#line 139 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("**"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 39:
#line 140 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 40:
#line 143 "rpal.y"
    { 
  (yyval.scmval) = scm_list_4(scm_str2symbol("at"), (yyvsp[(1) - (4)].scmval), (yyvsp[(3) - (4)].scmval), (yyvsp[(4) - (4)].scmval));
}
    break;

  case 41:
#line 146 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 42:
#line 149 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("gamma"), (yyvsp[(1) - (2)].scmval), (yyvsp[(2) - (2)].scmval)); }
    break;

  case 43:
#line 150 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 44:
#line 153 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 45:
#line 154 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 46:
#line 155 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 47:
#line 156 "rpal.y"
    { (yyval.scmval) = scm_list_1(scm_str2symbol("true")); }
    break;

  case 48:
#line 157 "rpal.y"
    { (yyval.scmval) = scm_list_1(scm_str2symbol("false")); }
    break;

  case 49:
#line 158 "rpal.y"
    { (yyval.scmval) = scm_list_1(scm_str2symbol("nil")); }
    break;

  case 50:
#line 159 "rpal.y"
    { (yyval.scmval) = (yyvsp[(2) - (3)].scmval); }
    break;

  case 51:
#line 160 "rpal.y"
    { (yyval.scmval) = scm_list_1(scm_str2symbol("dummy")); }
    break;

  case 52:
#line 163 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("within"), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 53:
#line 164 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 54:
#line 167 "rpal.y"
    { (yyval.scmval) = scm_cons(scm_str2symbol("and"), scm_cons((yyvsp[(1) - (2)].scmval),(yyvsp[(2) - (2)].scmval))); }
    break;

  case 55:
#line 168 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 56:
#line 171 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(2) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 57:
#line 174 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(2) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 58:
#line 175 "rpal.y"
    { (yyval.scmval) = SCM_EOL; }
    break;

  case 59:
#line 178 "rpal.y"
    { (yyval.scmval) = scm_list_2(scm_str2symbol("rec"), (yyvsp[(2) - (2)].scmval)); }
    break;

  case 60:
#line 179 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 61:
#line 182 "rpal.y"
    { (yyval.scmval) = scm_list_3(scm_str2symbol("="), (yyvsp[(1) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 62:
#line 183 "rpal.y"
    { 
  (yyval.scmval) = scm_list_4(scm_str2symbol("fcn_form"), (yyvsp[(1) - (4)].scmval), (yyvsp[(2) - (4)].scmval), (yyvsp[(4) - (4)].scmval)); 
}
    break;

  case 63:
#line 186 "rpal.y"
    { (yyval.scmval) = (yyvsp[(2) - (3)].scmval); }
    break;

  case 64:
#line 189 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 65:
#line 190 "rpal.y"
    { (yyval.scmval) = (yyvsp[(2) - (3)].scmval); }
    break;

  case 66:
#line 191 "rpal.y"
    { (yyval.scmval) = scm_list_1(scm_str2symbol("empty")); }
    break;

  case 67:
#line 194 "rpal.y"
    { (yyval.scmval) = scm_cons(scm_str2symbol("comma"),
						scm_cons((yyvsp[(1) - (2)].scmval), (yyvsp[(2) - (2)].scmval))); }
    break;

  case 68:
#line 196 "rpal.y"
    { (yyval.scmval) = (yyvsp[(1) - (1)].scmval); }
    break;

  case 69:
#line 199 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(2) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 70:
#line 202 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(2) - (3)].scmval), (yyvsp[(3) - (3)].scmval)); }
    break;

  case 71:
#line 203 "rpal.y"
    { (yyval.scmval) = SCM_EOL; }
    break;

  case 72:
#line 206 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(1) - (2)].scmval), (yyvsp[(2) - (2)].scmval)); }
    break;

  case 73:
#line 209 "rpal.y"
    { (yyval.scmval) = scm_cons((yyvsp[(1) - (2)].scmval), (yyvsp[(2) - (2)].scmval)); }
    break;

  case 74:
#line 210 "rpal.y"
    { (yyval.scmval) = SCM_EOL; }
    break;

  case 75:
#line 213 "rpal.y"
    {(yyval.scmval) = scm_list_2(scm_str2symbol("<identifier>"), (yyvsp[(1) - (1)].scmval));}
    break;

  case 76:
#line 216 "rpal.y"
    { (yyval.scmval) = scm_list_2(scm_str2symbol("<string>"), (yyvsp[(1) - (1)].scmval)); }
    break;

  case 77:
#line 219 "rpal.y"
    { (yyval.scmval) = scm_list_2(scm_str2symbol("<integer>"), (yyvsp[(1) - (1)].scmval)); }
    break;


/* Line 1267 of yacc.c.  */
#line 1916 "rpal.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 221 "rpal.y"


#ifndef HAVE_LIBREADLINE
char *readline(const char* prompt) {
  static char buf[65536];
  printf("%s", prompt);
  if(fgets(buf, 65536, stdin) != NULL) {
    if(buf[strlen(buf)-1] == '\n') buf[strlen(buf)-1] = '\0';
    if(buf[strlen(buf)-1] == '\r') buf[strlen(buf)-1] = '\0';
    return buf;
  }
  return NULL;
}

void add_history(const char* line) {}
#endif

int main(int argc, char *argv[]) {
  unsigned mode = 0;
  FILE *infile = NULL;
  int option_index = 0;
  SCM runlib, stree;

  while(1) {
    static struct option long_options[] = {
      {"ast", 0, NULL, 'a'},
      {"st", 0, NULL, 's'},
      {"run", 0, NULL, 'r'},
      {"interactive", 0, NULL, 'i'},
      {"help", 0, NULL, 'h'},
      {"version", 0, NULL, 'V'},
      {NULL, 0, NULL, 0}};
    char c;
    
    c = getopt_long(argc, argv, "asrihV", long_options, &option_index);
    if(c == -1) {
      if((optind < argc) && !(mode & 8)) {
	infile = fopen(argv[optind], "r");
	if(infile == NULL) {
	  fprintf(stderr, "Fatal: can't open %s for reading: %s\n", 
		  argv[optind], strerror(errno));
	  exit(3);
	} else yyrestart(infile);
      }
      break;
    }

    switch(c) {
    case 'a': mode = mode | 1; break;
    case 's': mode = mode | 2; break;
    case 'r': mode = mode | 4; break;
    case 'i': mode = mode | 8; break;
    case 'h':
    case '?':
      printf("Syntax: rpal [options] [file]\n"
             "Options:\n"
	     "-a, --ast          Output the abstract syntax tree.\n"
             "-s, --st           Output the scheme tree.\n"
	     "-i, --interactive  Run an interactive read-eval-print loop.\n"
             "-r, --run          When combined with -a or -s, interpret the\n"
	     "                   program rather than exiting after disp-\n"
             "                   laying the requested information. No effect\n"
             "                   when used by itself.\n"
	     "-h, --help         This cruft.\n"
             "-V, --version      Display version information and exit.\n"
	     "\n"
	     "If no file is supplied, input is read from stdin.\n"
	     "Exit status: 0 on success, 1 on syntax error, 2 on semantic\n"
	     "or runtime error, 3 on other error.\n");
      exit(0);
    case 'V': printf("%s\n", PACKAGE_STRING); exit(0);
    }
  }

  scm_init_guile();
  scm_c_use_module("ice-9 pretty-print");
  scm_c_use_module("ice-9 syncase");

  runlib = scm_sys_search_load_path(scm_makfrom0str("rpal.scm"));
  if(runlib == SCM_BOOL_F) {
    fprintf(stderr, "Fatal: unable to locate rpal.scm runtime library.\n");
    exit(3);
  }
  scm_primitive_load(runlib);
  
  if(mode & 8) { /* Interactive mode */ 
    while(1) {
      FILE *in, *out;
      int fd[2];
      char *str; 
      str = (char*)readline("rpal> ");
      
      if(str != NULL) add_history(str);
      else exit(0);
    
      if(!strcmp(str, "ast")) { mode = mode ^ 1; FREE_LINE(str); continue; }
      if(!strcmp(str, "st")) { mode = mode ^ 2; FREE_LINE(str); continue; }
      if(!strcmp(str, "")) { FREE_LINE(str); continue; }
      if(!strcmp(str, "quit")) { exit(0); }

      if(pipe(fd) == -1) {
	perror("pipe");
	exit(3);
      }

      in = fdopen(fd[0], "r");
      if(in == NULL) {
	perror("fdopen");
	exit(3);
      }
      
      out = fdopen(fd[1], "w");
      if(out == NULL) {
	perror("fdopen");
	exit(3);
      }

      if(fwrite(str, sizeof (char), strlen(str), out) == -1) {
	perror("fwrite");
	exit(3);
      }

      FREE_LINE(str);
      fclose(out);

      yyrestart(in);
      if(yyparse()) {
	fclose(in);
	continue;
      }

      if(mode&1)
	scm_eval(scm_list_2(scm_str2symbol("pretty-print"),
			    scm_list_2(scm_sym_quote, astree)),
		 scm_interaction_environment());
      
      stree = scm_eval(scm_list_2(scm_str2symbol("gen-scm-tree"),
				  scm_list_2(scm_sym_quote, astree)),
		       scm_interaction_environment());
      if(stree == SCM_BOOL_F) continue;

      if(mode&2) scm_eval(scm_list_2(scm_str2symbol("pretty-print"), 
				 scm_list_2(scm_sym_quote, stree)),
		      scm_interaction_environment());

      scm_eval(scm_list_2(scm_str2symbol("interactive-eval"), 
                          scm_list_2(scm_str2symbol("quote"), stree)),
	       scm_interaction_environment());
      printf("\n");
    }
    exit(0);
  }

  /*Non-interactive mode*/
  if(yyparse()) exit(1);
  if(infile != NULL) fclose(infile);
  if(mode&1) { scm_eval(scm_list_2(scm_str2symbol("pretty-print"),
				   scm_list_2(scm_sym_quote, astree)),
			scm_interaction_environment());}
  if(mode&2 || !(mode&1)) {
    stree = scm_eval(scm_list_2(scm_str2symbol("gen-scm-tree"),
				scm_list_2(scm_sym_quote, astree)),
		     scm_interaction_environment());
    if(stree == SCM_BOOL_F) exit(2);
  }
  if(mode&2) scm_eval(scm_list_2(scm_str2symbol("pretty-print"), 
				 scm_list_2(scm_sym_quote, stree)),
		      scm_interaction_environment());
  if(mode&4 || !(mode&3)) {
    scm_eval(stree, scm_interaction_environment());
    printf("\n");
  }
  return 0;
}

