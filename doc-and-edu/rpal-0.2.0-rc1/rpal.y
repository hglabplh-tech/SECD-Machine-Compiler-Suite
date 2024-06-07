/* RPAL - An interpreter for the Right-reference Pedagogic Algorithmic Language
 * Copyright (C) 2006 Daniel Franke
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

%{
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
%}

%union {
  SCM scmval;
}

%token <scmval> T_LET T_IN T_FN T_WHERE T_AUG T_OR T_NOT T_GR T_GE T_LS T_LE 
%token <scmval> T_EQ T_NE T_TRUE T_FALSE T_NIL T_DUMMY T_WITHIN T_AND T_REC 
%token <scmval> T_POW T_ARROW T_IDENTIFIER T_STRING T_INTEGER T_OPERATOR

%type <scmval> Expr ExprWhere Tuple TupleRest TupleRest2 AugmentedTuple 
%type <scmval> BasicTuple Boolean BooleanT BooleanS BooleanP Arith ArithT
%type <scmval> ArithF ArithP Rator Rand Def DefA DefARest DefARest2 DefRec DefB
%type <scmval> VarBasic VarList VarListRest VarListRest2 VarPlus VarPlusRest
%type <scmval> Identifier String Integer
%%

Program: Expr { astree = $1; }
;

Expr: T_LET Def T_IN Expr { $$ = scm_list_3(scm_str2symbol("let"), $2, $4); }
| T_FN VarPlus '.' Expr { $$ = scm_list_3(scm_str2symbol("lambda"), $2, $4); }
| ExprWhere { $$ = $1; }
;

ExprWhere: Tuple T_WHERE DefRec { $$ = scm_list_3(scm_str2symbol("where"),
						  $1, $3); }
| Tuple { $$ = $1; }
;

Tuple: AugmentedTuple TupleRest { 
  $$ = scm_append(scm_list_2(scm_list_2(scm_str2symbol("tau"), $1), $2));
}
| AugmentedTuple { $$ = $1; }
;

TupleRest: ',' AugmentedTuple TupleRest2 { $$ = scm_cons($2, $3); }
;

TupleRest2: ',' AugmentedTuple TupleRest2 { $$ = scm_cons($2, $3); }
| { $$ = SCM_EOL; }
;

AugmentedTuple: AugmentedTuple T_AUG BasicTuple { 
  $$ = scm_list_3(scm_str2symbol("aug"), $1, $3);
}
| BasicTuple { $$ = $1; }
;

BasicTuple: Boolean T_ARROW BasicTuple '|' BasicTuple { 
  $$ = scm_list_4(scm_str2symbol("ternary"), $1, $3, $5);
}
| Boolean { $$ = $1; }
;

Boolean: Boolean T_OR BooleanT { $$ = scm_list_3(scm_str2symbol("or"), 
						 $1, $3); }
| BooleanT { $$ = $1; }
;

BooleanT: BooleanT '&' BooleanS { $$ = scm_list_3(scm_str2symbol("&"), 
						  $1, $3); }
| BooleanS { $$ = $1; }
;

BooleanS: T_NOT BooleanP { $$ = scm_list_2(scm_str2symbol("not"), $2); }
| BooleanP { $$ = $1; }
;

BooleanP: Arith T_GR Arith { $$ = scm_list_3(scm_str2symbol("gr"), $1, $3); }
| Arith T_GE Arith { $$ = scm_list_3(scm_str2symbol("ge"), $1, $3); }
| Arith T_LS Arith { $$ = scm_list_3(scm_str2symbol("ls"), $1, $3); }
| Arith T_LE Arith { $$ = scm_list_3(scm_str2symbol("le"), $1, $3); }
| Arith T_EQ Arith { $$ = scm_list_3(scm_str2symbol("eq"), $1, $3); }
| Arith T_NE Arith { $$ = scm_list_3(scm_str2symbol("ne"), $1, $3); }
| Arith { $$ = $1; }
;

Arith: Arith '+' ArithT { $$ = scm_list_3(scm_str2symbol("+"), $1, $3); }
| Arith '-' ArithT { $$ = scm_list_3(scm_str2symbol("-"), $1, $3); }
| '+' ArithT { $$ = $2; }
| '-' ArithT { $$ = scm_list_2(scm_str2symbol("neg"), $2); }
| ArithT { $$ = $1; }
;

ArithT: ArithT '*' ArithF { $$ = scm_list_3(scm_str2symbol("*"), $1, $3); }
| ArithT '/' ArithF { $$ = scm_list_3(scm_str2symbol("/"), $1, $3); }
| ArithF { $$ = $1; }
;

ArithF: ArithP T_POW ArithF { $$ = scm_list_3(scm_str2symbol("**"), $1, $3); }
| ArithP { $$ = $1; }
;

ArithP: ArithP '@' Identifier Rator { 
  $$ = scm_list_4(scm_str2symbol("at"), $1, $3, $4);
}
| Rator { $$ = $1; }
;

Rator: Rator Rand { $$ = scm_list_3(scm_str2symbol("gamma"), $1, $2); }
| Rand { $$ = $1; }
;

Rand: Identifier { $$ = $1; }
| Integer { $$ = $1; }
| String { $$ = $1; }
| T_TRUE { $$ = scm_list_1(scm_str2symbol("true")); }
| T_FALSE { $$ = scm_list_1(scm_str2symbol("false")); }
| T_NIL { $$ = scm_list_1(scm_str2symbol("nil")); }
| '(' Expr ')' { $$ = $2; }
| T_DUMMY { $$ = scm_list_1(scm_str2symbol("dummy")); }
;

Def: DefA T_WITHIN Def { $$ = scm_list_3(scm_str2symbol("within"), $1, $3); }
| DefA { $$ = $1; }
;

DefA: DefRec DefARest { $$ = scm_cons(scm_str2symbol("and"), scm_cons($1,$2)); }
| DefRec { $$ = $1; }
;

DefARest: T_AND DefRec DefARest2 { $$ = scm_cons($2, $3); }
; 

DefARest2: T_AND DefRec DefARest2 { $$ = scm_cons($2, $3); }
| { $$ = SCM_EOL; }
;

DefRec: T_REC DefB { $$ = scm_list_2(scm_str2symbol("rec"), $2); }
| DefB { $$ = $1; }
;

DefB: VarList '=' Expr { $$ = scm_list_3(scm_str2symbol("="), $1, $3); }
| Identifier VarPlus '=' Expr { 
  $$ = scm_list_4(scm_str2symbol("fcn_form"), $1, $2, $4); 
}
| '(' Def ')' { $$ = $2; }
;

VarBasic: Identifier { $$ = $1; }
| '(' VarList ')' { $$ = $2; }
| '(' ')' { $$ = scm_list_1(scm_str2symbol("empty")); }
;

VarList: Identifier VarListRest { $$ = scm_cons(scm_str2symbol("comma"),
						scm_cons($1, $2)); }
| Identifier { $$ = $1; }
;

VarListRest: ',' Identifier VarListRest2 { $$ = scm_cons($2, $3); }
;

VarListRest2: ',' Identifier VarListRest2 { $$ = scm_cons($2, $3); }
| { $$ = SCM_EOL; }
;

VarPlus: VarBasic VarPlusRest { $$ = scm_cons($1, $2); }
;

VarPlusRest: VarBasic VarPlusRest { $$ = scm_cons($1, $2); }
| { $$ = SCM_EOL; }
;  

Identifier: T_IDENTIFIER {$$ = scm_list_2(scm_str2symbol("<identifier>"), $1);}
;

String: T_STRING { $$ = scm_list_2(scm_str2symbol("<string>"), $1); }
;

Integer: T_INTEGER { $$ = scm_list_2(scm_str2symbol("<integer>"), $1); }
;
%%

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
