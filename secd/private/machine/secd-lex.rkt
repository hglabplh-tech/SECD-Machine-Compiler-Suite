#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide value-tokens op-tokens next-token)

(define-tokens value-tokens (param-id))

(define-empty-tokens op-tokens (EOL Right_Parenthesis Left_Parenthesis Right_Square_Bracket
                                       Left_Square_Bracket lambda-calc apply-fun def-define
                                       cond-branch where is? heap-alloc heap-free heap-get-at
                                       token-add heap-set-at! native-scheme add sub mul div
                                       sqrt is-eq? gt? lt? gt-eq? lt-eq? >> <<
                                       bit-and bit-not bit-ior bit-xor
                                       boolean? integer? number? string? char? byte? list? cons? procedure?
                                       WS EOF ))


(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [#\) (token-Right_Parenthesis)]
   [#\( (token-Left_Parenthesis)]
   [#\] (token-Right_Square_Bracket)]
   [#\[ (token-Left_Square_Bracket)]
   ["define" (token-def-define)]
   ["where-cond" (token-where)]
   ["is?" (token-is?)]
   ["lambda" (token-lambda-calc)]
   ["cond-branch" (token-cond-branch)]
   ["apply-fun" (token-apply-fun)]
   ["heap-alloc" (token-heap-alloc)]
   ["heap-get-at" (token-heap-get-at)]
   ["heap-set-at!" (token-heap-set-at!)]
   ["native-scheme" (token-native-scheme)]
   ["add" (token-add)] ;; this part follows
   ["sub" (token-sub)]
   ["mul" (token-mul)]
   ["div" (token-div)]
   ["sqrt" (token-sqrt)]
   ["is-eq?" (token-is-eq?)]
   ["gt?" (token-gt?)]
   ["lt?" (token-lt?)]
   ["gt-eq?" (token-gt-eq?)]
   ["lt-eq?" (token-lt-eq?)]
   [">>" (token->>)]
   ["<<" (token-<<)]
   ["bit-and" (token-bit-and)]
   ["bit-not" (token-bit-not)]
   ["bit-ior" (token-bit-ior)]
   ["bit-xor" (token-bit-xor)]
   ["boolean?" (token-boolean?)]
   ["integer?" (token-integer?)]
   ["number?" (token-number?)]
   ["string?" (token-string?)]
   ["char?" (token-char?)]
   ["byte?" (token-byte?)]
   ["list?" (token-list?)]
   ["cons?" (token-cons?)]
   ["procedure?" (token-procedure?)]  
   [(:: alphabetic  (:or alphabetic (char-range #\0 #\9) #\- #\_ #\? #\! #\/ #\> #\< #\: #\& #\$ #\% #\@ #\+ #\- #\* #\# )) (token-param-id)]
   [(:: whitespace) (token-WS)]))
   

