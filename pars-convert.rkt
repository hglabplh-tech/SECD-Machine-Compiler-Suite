#lang racket
(require parser-tools/yacc-to-scheme)

(trans (string->path "./Fortran77Lexer.g4"))
