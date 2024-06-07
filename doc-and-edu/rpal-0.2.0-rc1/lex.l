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
#include <libguile.h>
#include "rpal.h"

  static int lineno = 1;
  void process_escapes(char* text);
  int rule(int ttype); 
%}

LETTER          [A-Za-z]
DIGIT 		[0-9]
OPER 		[]+*<>&.@/:=~|$!#%^_[{}\"`?(),;-]

%%
"let" { return rule(T_LET); }
"in" { return rule(T_IN); }
"fn" { return rule(T_FN); }
"where" { return rule(T_WHERE); }
"aug" { return rule(T_AUG); }
"or" { return rule(T_OR); }
"not" { return rule(T_NOT); }
"gr" { return rule(T_GR); }
"ge" { return rule(T_GE); }
"ls" { return rule(T_LS); }
"le" { return rule(T_LE); }
"eq" { return rule(T_EQ); }
"ne" { return rule(T_NE); }
"true" { return rule(T_TRUE); }
"false" { return rule(T_FALSE); }
"nil" { return rule(T_NIL); }
"dummy" { return rule(T_DUMMY); }
"within" { return rule(T_WITHIN); }
"and" { return rule(T_AND); }
"rec" { return rule(T_REC); }
">=" { return rule(T_GE); }
">" { return rule(T_GR); }
"<=" { return rule(T_LE); }
"<" { return rule(T_LS); }
"**" { return rule(T_POW); }
"->" { return rule(T_ARROW); }

{LETTER}({LETTER}|{DIGIT}|_)* { return rule(T_IDENTIFIER); }
{DIGIT}+ 	              { return rule(T_INTEGER); }
({OPER}) 	              { return rule(yytext[0]); }
\'(\\\\|\\\'|[^\'])*\'        { return rule(T_STRING); }
(\r\n|\n|\r) { lineno++; }
([ ]|\t) ;
"//".* ; /* Note that this is a pedantic violation of the RPAL spec since it
	    allows characters that are not in RPAL's alphabet. */
%%

void yyerror(const char *msg) {
  printf("%d: %s at \'%s\'\n", lineno, msg, yytext);
}

/* Processing escapes for a string consists of removing the quotes and
 * converting \t, \n, \", \', and \\. Since the string is always shorter
 * after escape processing, we can do the processing in-place.
 */
void process_escapes(char *text) {
  char *oldchr = text+1;
  char *newchr = text;

  while(1) {
    if(*oldchr == '\\') {
      oldchr++;
      if(*oldchr == 't') *newchr++ = '\t';
      else if(*oldchr == 'n') *newchr++ = '\n';
      else if(*oldchr == '\\') *newchr++ = '\\';
      else if(*oldchr == '\"') *newchr++ = '\"';
      else if(*oldchr == '\'') *newchr++ = '\'';
      else {
	static char err[80];
	sprintf(err, "Warning: Unrecognized escape sequence \\%c will be "
                     "scanned as literal %c." , *oldchr, *oldchr);
	yyerror(err);
      }
      *oldchr++;
    } 
    else if(*oldchr == '\'' || /*This*/ *oldchr == '\0'/*shouldn't happen*/) {
      *newchr = '\0';
      return;
    }
    else *newchr++ = *oldchr++;
  }
}

/* Initialize yylval to a two-element list. The first element is the lexeme
 * and the second is the line on which it occurs. Process escapes on strings
 * and convert integers.  All other lexemes are left alone.
 */
int rule(int ttype) {
  /*fprintf(stderr, "%d: %s\n", ttype, yytext);*/
  switch(ttype) {
  case T_INTEGER: 
    yylval.scmval = scm_list_2(scm_string_to_number(scm_makfrom0str(yytext),
						   SCM_MAKINUM(10)),
			      SCM_MAKINUM(lineno));
    break;
  case T_STRING: process_escapes(yytext); /* Fall through */
  default: yylval.scmval = scm_list_2(scm_makfrom0str(yytext), 
				      SCM_MAKINUM(lineno));
  }

  return ttype;
}
