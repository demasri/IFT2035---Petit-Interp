#! /usr/bin/env gsi -:dar

;;; Fichier : petit-interp.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la premiere section.

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction parse-and-execute
;;; doit etre definie, et vous pouvez modifier et ajouter des
;;; definitions de fonction afin de bien decomposer le traitement a
;;; faire en petites fonctions.  Il faut vous limiter au sous-ensemble
;;; *fonctionnel* de Scheme dans votre codage (donc n'utilisez pas
;;; set!, set-car!, vector-set!, list-set!, begin, print, display,
;;; etc).

;; La fonction parse-and-execute recoit en parametre une liste des
;; caracteres qui constituent le programme a interpreter.  La
;; fonction retourne une chaine de caracteres qui sera imprimee comme
;; resultat final du programme.  S'il y a une erreur lors de
;; l'analyse syntaxique ou lors de l'execution, cette chaine de
;; caracteres contiendra un message d'erreur pertinent.  Sinon, la
;; chaine de caracteres sera l'accumulation des affichages effectues
;; par les enonces "print" executes par le programme interprete.

(define parse-and-execute
  (lambda (inp)
    (parse inp execute)))

;; La fonction next-sym recoit deux parametres, une liste de
;; caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole.  La continuation
;; sera appelee avec deux parametres, la liste des caracteres restants
;; (apres le symbole analyse) et le symbole qui a ete lu (soit un
;; symbole Scheme ou une chaine de caractere Scheme dans le cas d'un
;; <id> ou un entier Scheme dans le cas d'un <int>).  S'il y a une
;; erreur d'analyse (tel un caractere inapproprie dans la liste de
;; caracteres) la fonction next-sym retourne une chaine de caracteres
;; indiquant une erreur de syntaxe, sans appeler la continuation.

(define next-sym
  (lambda (inp cont)
    (cond ((null? inp)
           (cont inp 'EOI)) ;; retourner symbole EOI a la fin de l'input
          ((blanc? (@ inp))
           (next-sym ($ inp) cont)) ;; sauter les blancs
          (else
           (let ((c (@ inp)))
             (cond ((chiffre? c)   (symbol-int inp cont))
                   ((lettre? c)    (symbol-id inp cont))
                   ((char=? c #\( ) (cont ($ inp) 'LPAR))
                   ((char=? c #\) ) (cont ($ inp) 'RPAR))
                   ((char=? c #\; )  (cont ($ inp) 'SEMI))
                   ((char=? c #\+ )  (cont ($ inp) 'PLUS))
                   ((char=? c #\- )  (cont ($ inp) 'MINUS))
                   ((char=? c #\* )  (cont ($ inp) 'MULT))
                   ((char=? c #\/ )  (cont ($ inp) 'DIV))
                   ((char=? c #\% )  (cont ($ inp) 'MOD))
                   ((char=? c #\{ )  (cont ($ inp) 'LBRAQ))
                   ((char=? c #\} )  (cont ($ inp) 'RBRAQ))
                   ((char=? c #\= )  (cont ($ inp) 'EQ))
                   ((char=? c #\> )  (cont ($ inp) 'GREATER))
                   ((char=? c #\< )  (cont ($ inp) 'LESS))
                   ((char=? c #\! )  (cont ($ inp) 'NOT))
                   (else
                    (syntax-error 1))))))))

;; La fonction @ prend une liste de caractere possiblement vide et
;; retourne le premier caractere, ou le caractere #\nul si la liste
;; est vide.

(define @
  (lambda (inp)
    (if (null? inp) #\nul (car inp))))

;; La fonction $ prend une liste de caractere possiblement vide et
;; retourne la liste des caracteres suivant le premier caractere s'il
;; y en a un.

(define $
  (lambda (inp)
    (if (null? inp) '() (cdr inp))))

;; La fonction syntax-error retourne le message d'erreur indiquant une
;; erreur de syntaxe.

(define syntax-error
  (lambda (champs)
    (case champs
      ((1)
        "syntax error: symbole non reconnue\n")
      ((2)
        "syntax error: expected token not found, from function expected\n")
      ((3)
        "syntax error: expected token not found\n")
      ((4)
        "syntax error: expected token '=' not found\n")
      (else
      "Generic syntax error\n"))))

;; La fonction blanc? teste si son unique parametre est un caractere
;; blanc.

(define blanc?
  (lambda (c)
    (or (char=? c #\space) (char=? c #\newline) (char=? c #\tab))))

;; La fonction chiffre? teste si son unique parametre est un caractere
;; numerique.

(define chiffre?
  (lambda (c)
    (and (char>=? c #\0) (char<=? c #\9))))

;; La fonction lettre? teste si son unique parametre est une lettre
;; minuscule.

(define lettre?
  (lambda (c)
    (and (char>=? c #\a) (char<=? c #\z))))

;; La fonction symbol-int recoit deux parametres, une liste de
;; caracteres qui debute par un chiffre et une continuation.  La liste
;; de caracteres sera analysee pour en extraire le symbole <int>.  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole <int> analyse et le symbole
;; <int> qui a ete lu (un entier Scheme qui est la valeur numerique du
;; symbole <int>).

(define symbol-int
  (lambda (inp cont)
    (symbol-int-aux inp cont 0)))

(define symbol-int-aux
  (lambda (inp cont n)
    (if (chiffre? (@ inp))
        (symbol-int-aux ($ inp)
                        cont
                        (+ (* 10 n) (- (char->integer (@ inp)) 48)))
        (cont inp n))))

;; La fonction symbol-id recoit deux parametres, une liste de
;; caracteres qui debute par une lettre minuscule et une continuation.
;; La liste de caracteres sera analysee pour en extraire le prochain
;; symbole (soit un mot cle comme "print" ou un <id>).  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole analyse et le symbole qui a
;; ete lu (soit un symbole Scheme, comme PRINT-SYM, ou une chaine de
;; caracteres Scheme qui correspond au symbole <id>).

(define symbol-id
  (lambda (inp cont)
    (symbol-id-aux inp cont '())))

(define symbol-id-aux
  (lambda (inp cont lst)
    (if (lettre? (@ inp))
        (symbol-id-aux ($ inp) cont (cons (@ inp) lst))
        (let ((id (list->string (reverse lst))))
          (cond ((string=? id "print")
                 (cont inp 'PRINT-SYM))
                ((string=? id "if")
                 (cont inp 'IF-SYM))
                ((string=? id "else")
                 (cont inp 'ELSE-SYM))
                ((string=? id "do")
                 (cont inp 'DO-SYM))
                ((string=? id "while")
                 (cont inp 'WHILE-SYM))
                (else
                 (cont inp id)))))))

;; La fonction expect recoit trois parametres, un symbole, une liste
;; de caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole qui doit etre le meme
;; que le premier parametre de la fonction.  Dans ce cas la
;; continuation sera appelee avec un parametre, la liste des
;; caracteres restants apres le symbole analyse.  Si le prochain
;; symbole n'est pas celui qui est attendu, la fonction expect
;; retourne une chaine de caracteres indiquant une erreur de syntaxe.

(define expect
  (lambda (expected-sym inp cont)
    (next-sym inp
              (lambda (inp sym)
                (if (equal? sym expected-sym)
                    (cont inp)
                    (syntax-error 2))))))

;; La fonction parse recoit deux parametres, une liste de caracteres
;; et une continuation.  La liste de caracteres sera analysee pour
;; verifier qu'elle est conforme a la syntaxe du langage.  Si c'est le
;; cas, la continuation sera appelee avec une S-expression qui
;; represente l'ASA du programme.  Sinon la fonction parse retourne
;; une chaine de caracteres indiquant une erreur de syntaxe.

(define parse
  (lambda (inp cont)
    (<program> inp ;; analyser un <program>
               (lambda (inp program)
                 (expect 'EOI ;; verifier qu'il n'y a rien apres
                         inp
                         (lambda (inp)
                          (cont program)))))))

;; Les fonctions suivantes, <program>, <stat>, ... recoivent deux
;; parametres, une liste de caracteres et une continuation.  La liste
;; de caracteres sera analysee pour verifier qu'elle debute par une
;; partie qui est conforme a la categorie correspondante de la
;; grammaire du langage.  Si c'est le cas, la continuation sera
;; appelee avec deux parametres : une liste des caracteres restants du
;; programme et une S-expression qui represente l'ASA de ce fragment
;; de programme.  Sinon ces fonctions retournent une chaine de
;; caracteres indiquant une erreur de syntaxe.

(define <program>
  (lambda (inp cont)
    (<stat> inp cont))) ;; analyser un <stat>

;;
;;
(define <stat>
  (lambda (inp cont)
    (next-sym inp
              (lambda (inp2 sym)
                (case sym ;; determiner quel genre de <stat>
                  ((PRINT-SYM)
                    (<print_stat> inp2 cont))
                  ((IF-SYM)
                    (<if_stat> inp2 cont))
                  ((DO-SYM)
                    (<do_stat> inp2 cont))
                  ((WHILE-SYM)
                    (<while_stat> inp2 cont))
                  ((LBRAQ)
                    (<seq_stat> inp2 cont))
                  (else
                    (<expr_stat> inp cont)))))))

;;
;;
(define <seq_stat>
  (lambda (inp cont)
      (<stat> inp ;;Recupere la suite de la sequence
           (lambda(inp2 stat1)
            (next-sym  inp2
                      (lambda(inp3 sym2)
                      (if (equal? sym2 'RBRAQ)
                          (cont inp3 (list 'SEQ stat1 (list 'EMPTY)))
                          (<seq_stat> inp2
                                      (lambda (inp seq)
                                        (cont inp (list 'SEQ stat1 seq)))))))))))

;;
;;
(define <while_stat>
  (lambda (inp cont)
    (<paren_expr>  inp
                  (lambda (inp2 test)
                    (<stat>  inp2
                            (lambda (inp3 stat)
                            (cont inp3 (list 'WHILE test stat))))))))

;;
;;
(define <do_stat>
  (lambda (inp cont)
    (<stat>  inp
            (lambda (inp2 stat)
                          (expect 'WHILE-SYM
                                  inp2
                                  (lambda(inp3)
                                    (<paren_expr> inp3
                                        (lambda (inp4 test)
                                          (expect 'SEMI
                                                  inp4
                                                  (lambda(inp4)
                                                  (cont inp4 (list 'DO stat test))))))))))))

;;
;;
(define <print_stat>
  (lambda (inp cont)
    (<paren_expr> inp ;; analyser un <paren_expr>
                  (lambda (inp expr)
                    (expect 'SEMI ;; verifier qu'il y a ";" apres
                            inp
                            (lambda (inp)
                              (cont inp
                                    (list 'PRINT expr))))))))

;;
;;
(define <if_stat>
  (lambda (inp cont)
    (<paren_expr> inp ;;Obtenir le test du if
            (lambda (inp2 test)
              (<stat>  inp2
                      (lambda (inp3 stat1)
                        (next-sym inp3
                          (lambda (inp4 sym1)
                            (if (equal? sym1 'ELSE-SYM)
                            (<stat> inp4
                                    (lambda(inp stat2)
                                      (cont inp (list 'IF test stat1 stat2))))
                            (cont inp3 (list 'IF test stat1)))))))))))

;;
;;
(define <paren_expr>
  (lambda (inp cont)
    (expect 'LPAR ;; doit debuter par "("
            inp
            (lambda (inp)
              (<expr> inp ;; analyser un <expr>
                      (lambda (inp expr)
                        (expect 'RPAR ;; doit etre suivi de ")"
                                inp
                                (lambda (inp)
                                  (cont inp
                                        expr)))))))))

;;
;;
(define <expr_stat>
  (lambda (inp cont)
    (<expr> inp ;; analyser un <expr>
            (lambda (inp expr)
              (expect 'SEMI ;; doit etre suivi de ";"
                      inp
                      (lambda (inp)
                        (cont inp
                              (list 'EXPR expr))))))))

;;
;;
(define <expr>
  (lambda (inp cont)
    (next-sym inp ;; verifier 1e symbole du <expr>
              (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <expr>
                          (lambda (inp3 sym2)
                            (next-sym inp3
                                      (lambda (inp4 sym3)
                                        (if (and (string? sym1) ;; combinaison "id =" et non "id =="
                                          (equal? sym2 'EQ) (not (equal? sym3 'EQ)))
                                            (<expr> inp3
                                              (lambda (inp5 expr)
                                                (cont inp5
                                                  (list 'ASSIGN
                                                        sym1
                                                        expr))))
                                          (<test> inp cont))))))))))

;;
;;
(define <test>
  (lambda (inp cont)
    (<sum> inp ;;Recupere la premiere partie du test
            (lambda (inp2 sum1)
              (next-sym inp2 ;;Recupere le terme suivant la premiere partie
                       (lambda (inp3 sym2)
                        (next-sym inp3 ;;Recupere le deuxieme terme suivant la premiere partie
                                 (lambda(inp4 sym3)
                                  (case sym2
                                    ((GREATER)
                                    (if(equal? sym3 'EQ)
                                      (<sum> inp4
                                             (lambda(inp4 sum2)
                                                (cont inp4 (list 'GTEQ sum1 sum2))))

                                      (<sum> inp3
                                           (lambda(inp3 sum2)
                                              (cont inp3 (list 'GT sum1 sum2))))))
                                    ((LESS)
                                    (if(equal? sym3 'EQ)
                                      (<sum> inp4
                                           (lambda(inp4 sum2)
                                            (cont inp4 (list 'LTEQ sum1 sum2))))

                                      (<sum> inp3
                                         (lambda(inp3 sum2)
                                            (cont inp3 (list 'LT sum1 sum2))))))

                                    ((NOT)
                                    (if(equal? sym3 'EQ)
                                      (<sum> inp4
                                            (lambda(inp4 sum2)
                                              (cont inp4 (list 'NOTEQ sum1 sum2))))
                                      (syntax-error 3)))

                                    ((EQ)
                                    (if(equal? sym3 'EQ)
                                      (<sum> inp4
                                            (lambda(inp4 sum2)
                                              (cont inp4 (list 'EQEQ sum1 sum2))))
                                      (syntax-error 4)))

                                    (else
                                      (cont inp2 sum1)))))))))))

;;
;;
(define <sum>
  (lambda (inp cont)
    (<mult> inp ;;Recupere le premier terme
            (lambda(inp2 mult1)
              (next-sym  inp2 ;;Recupere le symbole suivant le premier terme
                        (lambda(inp3 mult2)
                          (case mult2
                            ((PLUS)
                              (<sum> inp3 (lambda(inp mult2)
                                            (cont inp (list 'ADD mult1 mult2)))))
                            ((MINUS)
                              (<sum> inp3 (lambda(inp mult2)
                                            (cont inp (list 'DIF mult1 mult2)))))
                            (else
                              (cont inp2 mult1)))))))))

;;
;;
(define <mult>
  (lambda (inp cont)
    (<term>  inp ;;Recupere le premier terme
                  (lambda(inp2 terme)
                    (next-sym  inp2 ;;Recupere le symbole suivant le premier terme
                              (lambda(inp3 sym2)
                                (if (or (equal? sym2 'MOD) (equal? sym2 'MULT) (equal? sym2 'DIV))
                                  (<mult_expr> inp cont terme) ;;Pour produire les expression de type MULT
                                  (cont inp2 terme))))))))

;;
;;
(define <mult_expr>
  (lambda (inp cont expr)
    (<term> inp ;;Recupere le premier terme
            (lambda(inp2 terme1)
              (next-sym  inp2 ;;Recupere le symbole suivant le premier terme
                        (lambda(inp3 sym2)
                          (if (or (equal? sym2 'MOD) (equal? sym2 'MULT) (equal? sym2 'DIV))
                                (<term> inp3 ;;Recupere le deuxieme terme
                                        (lambda(inp4 terme2)
                                          (<mult_expr> inp3  cont (list sym2 expr terme2))))
                                (cont inp2 expr))))))))

;;
;;
(define <term>
  (lambda (inp cont)
    (next-sym inp ;; verifier le premier symbole du <term>
              (lambda (inp2 sym)
                (cond ((string? sym) ;; identificateur?
                       (cont inp2 (list 'VAR sym)))
                      ((number? sym) ;; entier?
                       (cont inp2 (list 'INT sym)))
                      (else
                       (<paren_expr> inp cont)))))))

;; La fonction execute prend en parametre l'ASA du programme a
;; interpreter et retourne une chaine de caracteres qui contient
;; l'accumulation de tout ce qui est affiche par les enonces "print"
;; executes par le programme interprete.

(define execute
  (lambda (ast)
    (exec-stat '() ;; etat des variables globales
               ""  ;; sortie jusqu'a date
               ast ;; ASA du programme
               (lambda (env output)
                 output)))) ;; retourner l'output pour qu'il soit affiche

;; La fonction exec-stat fait l'interpretation d'un enonce du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'enonce a interpreter et une continuation.  La continuation sera
;; appelee avec deux parametres : une liste d'association donnant la
;; valeur de chaque variable du programme apres l'interpretation de
;; l'enonce et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'enonce.

(define exec-stat
  (lambda (env output ast cont)
    (case (car ast)

      ((SEQ) ;;Evalue se qui se trouve dans la liste
        (exec-stat env output (cadr ast)
                   (lambda (env output)
                    (exec-stat env output (caddr ast)
                                          (lambda (env output)
                                            cont env output)))))

      ((EMPTY)
        (cont env output))

      ((PRINT)
       (exec-expr env ;; evaluer l'expression du print
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env ;; ajouter le resultat a la sortie, rajouter une detection si variable non-declarer
                          (string-append output
                                         (number->string val)
                                         "\n")))))

      ((EXPR)
       (exec-expr env ;; evaluer l'expression
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env output)))) ;; continuer en ignorant le resultat

      (else
        (execution-error 5  (car ast))))))

;; La fonction exec-expr fait l'interpretation d'une expression du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'expression a interpreter et une continuation.  La continuation
;; sera appelee avec deux parametres : une liste d'association donnant
;; la valeur de chaque variable du programme apres l'interpretation de
;; l'expression et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'expression.

(define exec-expr
  (lambda (env output ast cont)
    (case (car ast)

      ((INT)
        (cont env output (cadr ast))) ;; retourner la valeur de la constante

      ((VAR)
        (eval  (cadr ast) env
                (lambda (val env1)
                (cont env1 output val)))) ;; retourner la valeur de la constante

      ((ASSIGN)
        (exec-expr  env output (caddr ast)
                    (lambda (env output val)
                      (if (assoc (cadr ast) env)
                        (cont (update-env (cadr ast) val env) output val)
                        (cont (cons (cons (cadr ast) val) env) output val)))))

      ((ADD)
        (arithmetic env output ast ;;Aller chercher les valeurs
                  (lambda(env output val1 val2)
                    (cont env output (+ val1 val2)))))
      ((DIF)
        (arithmetic env output ast ;;Aller chercher les valeurs
                  (lambda(env output val1 val2)
                    (cont env output (- val1 val2)))))

      ((MULT)
        (arithmetic env output ast ;;Aller chercher les valeurs
                  (lambda(env output val1 val2)
                    (cont env output (* val1 val2)))))

      ((DIV)
        (arithmetic env output ast ;;Aller chercher les valeurs
                  (lambda(env output val1 val2)
                    (cont env output (quotient val1 val2)))))

      ((MOD)
        (arithmetic env output ast ;;Aller chercher les valeurs
                  (lambda(env output val1 val2)
                    (cont env output (remainder val1 val2)))))

      (else
        (execution-error 4 (cadr ast))))))

;;La fonction Arithmetic permet d'aller chercher
;;les deux termes qui compose une action arithmetic (+, -, *, / et modulo)
(define arithmetic
  (lambda (env output ast cont)
    (exec-expr env output (cadr ast) ;;Aller chercher la premiere valeur
              (lambda(env output val1)
                (exec-expr  env output (caddr ast) ;;Aller chercher la deuxieme valeur
                            (lambda (env output val2)
                              (if (or (null? val1) (null? val2));;Detecter l'utilisation de variable non declarer
                              (execution-error 1)
                              (cont env output val1 val2))))))))

;;Fonction qui retourne la valeur pour une variable
;;donnee, et lui donne une valeur nulle dans le cas
;;ou celle-ci n'as pas encore ete declarer
(define eval
  (lambda (string env cont)
    (if (assoc string env)
      (cont (cdr (assoc string env)) env)
      (cont '() (cons (cons string  '()) env)))))

;;Fonction qui retourne le nouvel environment
;;avec la valeur desirer changer
(define update-env
  (lambda (var newVal env)
    (map (lambda(n)
            (if (equal? (car n) var)
              (cons var newVal)
              (cons (car n) (cdr n))))
          env)))

;;Fonction qui gere les les erreur d'execution
(define execution-error
  (lambda (champs)
    (case champs
      ((1)
        "execution error: using undefined variable\n")
      (else
      "internal error (unknown statement AST)\n"))))
;;;----------------------------------------------------------------------------

;;; *** NE MODIFIEZ PAS CETTE SECTION ***

(define main
  (lambda ()
    (print (parse (read-all (current-input-port) read-char) '()))))

;;;----------------------------------------------------------------------------
