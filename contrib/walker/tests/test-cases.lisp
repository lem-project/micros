((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS FOO NIL) NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS FOO NIL) NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS FOO NIL) NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 2))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 3))
 ((1 3 3) (1 2 3) (1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 2 3))
 ((1 3 3) (1 2 3) (1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 3 3))
 ((1 3 3) (1 2 3) (1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 4))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 0 1))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 1 1))
 ((0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (1 1 1))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 0 1 2))
 ((2 2) (1 1 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (1 0 1 2))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 1 1 2))
 ((0 1 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (1 1 1 2))
 ((2 2) (1 1 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (2 2))
 ((2 2) (1 1 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (3))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (0 0 1))
 ((2 2) (1 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (0 0 1 2))
 ((3 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (1 0 1 2))
 ((2 2) (1 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (2 2))
 ((2 2) (1 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (3 2))
 ((3 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (0 0 1))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (0 1 1))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 2))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 2))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 2))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 3))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 3))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (4 3))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 4))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 4))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 4))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (4 4))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 5))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 5))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 6))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 6))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 6))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 7))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 2 7))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 7))
 ((3 7)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (0 1))
 ((2) (1 5 1) (0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (1 1))
 ((1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (2 1))
 ((2 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (3 1))
 ((3 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (0 5 1))
 ((3) (0 5 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (1 5 1))
 ((2) (1 5 1) (0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (6 1))
 ((4) (6 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (0 8 1))
 ((5) (0 8 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (2))
 ((2) (1 5 1) (0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (3))
 ((3) (0 5 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (4))
 ((4) (6 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (5))
 ((5) (0 8 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (0 1 1))
 ((2 1) (1 5 1 1) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (1 1 1))
 ((1 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (2 1 1))
 ((2 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (3 1 1))
 ((3 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (0 5 1 1))
 ((3 1) (0 5 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (1 5 1 1))
 ((2 1) (1 5 1 1) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (6 1 1))
 ((4 1) (6 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (0 8 1 1))
 ((5 1) (0 8 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (2 1))
 ((2 1) (1 5 1 1) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (3 1))
 ((3 1) (0 5 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (4 1))
 ((4 1) (6 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (5 1))
 ((5 1) (0 8 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 0 1))
 ((0 4 0 1 2) (3 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 1 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 0 1 2))
 ((3 2) (0 2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 1 1 0 1 2))
 ((1 4 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (2 0 1 2))
 ((1 4 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (4 0 1 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (3 0 1 2))
 ((0 4 0 1 2) (3 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 4 0 1 2))
 ((0 4 0 1 2) (3 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 4 0 1 2))
 ((1 4 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 2 2))
 ((3 2) (0 2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 2 2))
 ((1 2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 2 2))
 ((1 2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (3 2))
 ((3 2) (0 2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 0 1))
 ((0 2 0 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 2 0 1))
 ((0 2 0 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 0 1 2))
 ((2 2) (0 3 0 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 1 1 0 1 2))
 ((1 3 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (2 0 1 2))
 ((1 3 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 3 0 1 2))
 ((2 2) (0 3 0 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (1 3 0 1 2))
 ((1 3 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (2 2))
 ((2 2) (0 3 0 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 0 1))
 ((0 2 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 1 0 1))
 ((1 2 0 1) (1 2 1 0 1) (0 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2 1 0 1))
 ((0 2 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (1 2 1 0 1))
 ((1 2 0 1) (1 2 1 0 1) (0 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2 0 1))
 ((0 2) (0 1 1) (0 2 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (1 2 0 1))
 ((1 2 0 1) (1 2 1 0 1) (0 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 1 1))
 ((0 2) (0 1 1) (0 2 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 1 1 1))
 ((1 2 1 1) (0 1 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2 1 1))
 ((0 2 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (1 2 1 1))
 ((1 2 1 1) (0 1 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2))
 ((0 2) (0 1 1) (0 2 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 0 1))
 ((3 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 0 1 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (3 2))
 ((3 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 1 4 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 0 1 4 2))
 ((2 4 2) (0 0 1 4 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (2 4 2))
 ((2 4 2) (0 0 1 4 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (5 2))
 ((5 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (6 2))
 ((6 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (WITH-HOGE NIL A B C))
  (0 0 1))
 ((2 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (LET ((A 0))
      A))
  (0 0 1 2))
 ((2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (LET ((A 0))
      A))
  (2 2))
 ((2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (LET ((A 0))
      A))
  (2 2))
 ((2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (LET ((A 0))
      A))
  (0 0 1 2))
 ((2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (WITH-HOGE NIL A B C))
  (0 0 1))
 ((2 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (6 2))
 ((6 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (5 2))
 ((5 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (2 4 2))
 ((2 4 2) (0 0 1 4 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 0 1 4 2))
 ((2 4 2) (0 0 1 4 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 1 4 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (3 2))
 ((3 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 0 1 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0))
    (DO ((X 1 (1+ X)))
        ((= 10 X))
      A
      (LET ((A 1))
        A)
      B
      C))
  (0 0 1))
 ((3 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2))
 ((0 2) (0 1 1) (0 2 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (1 2 1 1))
 ((1 2 1 1) (0 1 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2 1 1))
 ((0 2 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 1 1 1))
 ((1 2 1 1) (0 1 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 1 1))
 ((0 2) (0 1 1) (0 2 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (1 2 0 1))
 ((1 2 0 1) (1 2 1 0 1) (0 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2 0 1))
 ((0 2) (0 1 1) (0 2 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (1 2 1 0 1))
 ((1 2 0 1) (1 2 1 0 1) (0 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 2 1 0 1))
 ((0 2 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 1 0 1))
 ((1 2 0 1) (1 2 1 0 1) (0 1 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F (X &KEY (Y X))
             (G X))
           (G (Y)
             (F Y)))
    (G 10))
  (0 0 1))
 ((0 2 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (2 2))
 ((2 2) (0 3 0 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (1 3 0 1 2))
 ((1 3 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 3 0 1 2))
 ((2 2) (0 3 0 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (2 0 1 2))
 ((1 3 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 1 1 0 1 2))
 ((1 3 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 0 1 2))
 ((2 2) (0 3 0 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 2 0 1))
 ((0 2 0 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LABELS ((F ()
             (F)))
    (LABELS ((F (&OPTIONAL (X 1))
               X
               (F X)))
      #'F))
  (0 0 1))
 ((0 2 0 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (3 2))
 ((3 2) (0 2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 2 2))
 ((1 2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 2 2))
 ((1 2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 2 2))
 ((3 2) (0 2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 4 0 1 2))
 ((1 4 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 4 0 1 2))
 ((0 4 0 1 2) (3 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (3 0 1 2))
 ((0 4 0 1 2) (3 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (4 0 1 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (2 0 1 2))
 ((1 4 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 1 1 0 1 2))
 ((1 4 0 1 2) (2 0 1 2) (0 1 1 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 0 1 2))
 ((3 2) (0 2 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (1 1 2))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (FLET ((F ()
           ))
    (FLET ((F (&OPTIONAL (X 1))
             X
             #'F
             (F X)))
      (F X)
      #'F))
  (0 0 1))
 ((0 4 0 1 2) (3 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (5 1))
 ((5 1) (0 8 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (4 1))
 ((4 1) (6 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (3 1))
 ((3 1) (0 5 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (2 1))
 ((2 1) (1 5 1 1) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (0 8 1 1))
 ((5 1) (0 8 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (6 1 1))
 ((4 1) (6 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (1 5 1 1))
 ((2 1) (1 5 1 1) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (0 5 1 1))
 ((3 1) (0 5 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (3 1 1))
 ((3 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (2 1 1))
 ((2 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (1 1 1))
 ((1 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS #'(LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO)
                                        (0 1 1))
 ((2 1) (1 5 1 1) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (5))
 ((5) (0 8 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (4))
 ((4) (6 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (3))
 ((3) (0 5 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (2))
 ((2) (1 5 1) (0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (0 8 1))
 ((5) (0 8 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (6 1))
 ((4) (6 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (1 5 1))
 ((2) (1 5 1) (0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (0 5 1))
 ((3) (0 5 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (3 1))
 ((3 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (2 1))
 ((2 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (1 1))
 ((1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LAMBDA (X A B C &KEY (Y X) Z &AUX (FOO 10)) X Y Z FOO) (0 1))
 ((2) (1 5 1) (0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 7))
 ((3 7)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 2 7))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 7))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 6))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 6))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 6))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 5))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 5))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (4 4))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 4))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 4))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 4))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (4 3))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (3 3))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 3))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 2))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (2 2))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (1 2))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (0 1 1))
 ((2 2 7) (2 6) (2 5) (3 4) (2 4) (3 3) (2 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 0) (B 1))
    (LOAD-TIME-VALUE A B)
    (MULTIPLE-VALUE-CALL 'F A B A)
    (SETQ A B
          B A)
    (PROGN A B)
    (MULTIPLE-VALUE-PROG1 A B A)
    (UNWIND-PROTECT A (THE INTEGER B) C))
  (0 0 1))
 ((1 7) (3 6) (1 6) (1 5) (4 4) (1 4) (4 3) (2 3) (1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (3 2))
 ((3 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (2 2))
 ((2 2) (1 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (1 0 1 2))
 ((2 2) (1 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (0 0 1 2))
 ((3 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((A 1))
    (LET ((B A))
      A
      B))
  (0 0 1))
 ((2 2) (1 0 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (3))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (2 2))
 ((2 2) (1 1 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (1 1 1 2))
 ((2 2) (1 1 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 1 1 2))
 ((0 1 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (1 0 1 2))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 0 1 2))
 ((2 2) (1 1 1 2) (0 0 1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (1 1 1))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 1 1))
 ((0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET* ((A 0) (B A))
    (LET* ((A A) (B A))
      A)
    A)
  (0 0 1))
 ((3) (1 0 1 2) (1 1 1) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 4))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 3 3))
 ((1 3 3) (1 2 3) (1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 2 3))
 ((1 3 3) (1 2 3) (1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 3))
 ((1 3 3) (1 2 3) (1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1 2))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS FOO NIL) NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS FOO NIL) NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS FOO NIL) NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (BLOCK FOO
    (RETURN-FROM FOO 10)
    (BLOCK FOO (RETURN-FROM FOO 20) (RETURN-FROM FOO 30))
    (RETURN-FROM FOO 40))
  (1))
 ((1 4) (1 2) (1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFUN F (X Y) X Y X Y) (6)) ((6) (4) (1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFUN F (X Y) X Y X Y) (5)) ((5) (3) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFUN F (X Y) X Y X Y) (4)) ((6) (4) (1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFUN F (X Y) X Y X Y) (3)) ((5) (3) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFUN F (X Y) X Y X Y) (1 2)) ((6) (4) (1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFUN F (X Y) X Y X Y) (0 2)) ((5) (3) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD (X Y) (+ X Y)) (2 3)) ((2 3) (1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD (X Y) (+ X Y)) (1 3)) ((1 3) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD (X Y) (+ X Y)) (1 2)) ((2 3) (1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD (X Y) (+ X Y)) (0 2)) ((1 3) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (DEFMETHOD MICROS/WALKER::ADD (MICROS/WALKER::X MICROS/WALKER::Y) (+ MICROS/WALKER::X MICROS/WALKER::Y))
  (2 3))
 ((2 3) (1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (DEFMETHOD MICROS/WALKER::ADD (MICROS/WALKER::X MICROS/WALKER::Y) (+ MICROS/WALKER::X MICROS/WALKER::Y))
  (1 3))
 ((1 3) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (DEFMETHOD MICROS/WALKER::ADD (MICROS/WALKER::X MICROS/WALKER::Y) (+ MICROS/WALKER::X MICROS/WALKER::Y))
  (1 2))
 ((2 3) (1 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (DEFMETHOD MICROS/WALKER::ADD (MICROS/WALKER::X MICROS/WALKER::Y) (+ MICROS/WALKER::X MICROS/WALKER::Y))
  (0 2))
 ((1 3) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD :BEFORE ((X INTEGER) (Y INTEGER)) (PRINT (LIST X Y)))
                                        (2 1 4))
 ((2 1 4) (0 1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD :BEFORE ((X INTEGER) (Y INTEGER)) (PRINT (LIST X Y)))
                                        (1 1 4))
 ((1 1 4) (0 0 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD :BEFORE ((X INTEGER) (Y INTEGER)) (PRINT (LIST X Y)))
                                        (0 1 3))
 ((2 1 4) (0 1 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (DEFMETHOD ADD :BEFORE ((X INTEGER) (Y INTEGER)) (PRINT (LIST X Y)))
                                        (0 0 3))
 ((1 1 4) (0 0 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  ((LAMBDA (MICROS/WALKER::A MICROS/WALKER::B) (+ MICROS/WALKER::A MICROS/WALKER::B)) 1 2) (2 2 0))
 ((2 2 0) (1 1 0)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  ((LAMBDA (MICROS/WALKER::A MICROS/WALKER::B) (+ MICROS/WALKER::A MICROS/WALKER::B)) 1 2) (1 2 0))
 ((1 2 0) (0 1 0)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  ((LAMBDA (MICROS/WALKER::A MICROS/WALKER::B) (+ MICROS/WALKER::A MICROS/WALKER::B)) 1 2) (1 1 0))
 ((2 2 0) (1 1 0)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  ((LAMBDA (MICROS/WALKER::A MICROS/WALKER::B) (+ MICROS/WALKER::A MICROS/WALKER::B)) 1 2)
  (0 1 0))
 ((1 2 0) (0 1 0)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((COMMON-LISP-USER::A 0))
    COMMON-LISP-USER::A
    ((LAMBDA (COMMON-LISP-USER::A) (DECLARE (SPECIAL COMMON-LISP-USER::A)) COMMON-LISP-USER::A))
    ((LAMBDA (COMMON-LISP-USER::A) (DECLARE (SPECIAL COMMON-LISP-USER::A)) COMMON-LISP-USER::A))
    COMMON-LISP-USER::A)
  (3 0 3))
 ((3 0 4) (0 1 0 4) (3 0 3) (0 1 0 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((X 0))
    (LOOP (F X)))
  (1 1 2))
 ((1 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((X 0))
    (LOOP (F X)))
  (0 0 1))
 ((1 1 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :WITH Y := X
        :WITH Z := (F X Y))
  (2 12))
 ((6) (2 12)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :WITH Y := X
        :WITH Z := (F X Y))
  (1 12))
 ((2) (8) (1 12)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :WITH Y := X
        :WITH Z := (F X Y))
  (8))
 ((2) (8) (1 12)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :WITH Y := X
        :WITH Z := (F X Y))
  (10))
 ((10)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :WITH Y := X
        :WITH Z := (F X Y))
  (6))
 ((6) (2 12)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :WITH Y := X
        :WITH Z := (F X Y))
  (2))
 ((2) (8) (1 12)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :DO (F X))
  (1 6))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH X := 0
        :DO (F X))
  (2))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LOOP :WITH X := 0 :RETURN :IT :RETURN (F X)) (6))
 NIL)
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LOOP :WITH X := 0 :RETURN :IT :RETURN (F X)) (1 8))
 ((1 8) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS (LOOP :WITH X := 0 :RETURN :IT :RETURN (F X)) (2))
 ((1 8) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH ((X Y) . Z) := (F)
        :WITH A := (+ X Y Z))
  (3 8))
 ((2 2) (3 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH ((X Y) . Z) := (F)
        :WITH A := (+ X Y Z))
  (2 8))
 ((1 0 2) (2 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH ((X Y) . Z) := (F)
        :WITH A := (+ X Y Z))
  (1 8))
 ((0 0 2) (1 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH ((X Y) . Z) := (F)
        :WITH A := (+ X Y Z))
  (2 2))
 ((2 2) (3 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH ((X Y) . Z) := (F)
        :WITH A := (+ X Y Z))
  (1 0 2))
 ((1 0 2) (2 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH ((X Y) . Z) := (F)
        :WITH A := (+ X Y Z))
  (0 0 2))
 ((0 0 2) (1 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH (X . Y) := (F)
        :WITH A := X
        :WITH B := Y)
  (12))
 ((2 2) (12)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH (X . Y) := (F)
        :WITH A := X
        :WITH B := Y)
  (8))
 ((0 2) (8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH (X . Y) := (F)
        :WITH A := X
        :WITH B := Y)
  (2 2))
 ((2 2) (12)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH (X . Y) := (F)
        :WITH A := X
        :WITH B := Y)
  (0 2))
 ((0 2) (8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO := NIL
        :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (1 10))
 ((1 10) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO := NIL
        :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (6))
 ((1 10) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO := NIL
        :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (2))
 ((2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO
        :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (1 8))
 ((1 8) (4)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO
        :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (4))
 ((1 8) (4)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO
        :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (2))
 ((2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (1 6))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :IN '(1 2 3)
        :DO (PRINT X))
  (2))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :IN (LIST A) :BY FN
        :DO (PRINT X))
  (1 14))
 ((1 14) (8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :IN (LIST A) :BY FN
        :DO (PRINT X))
  (12))
 ((12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :IN (LIST A) :BY FN
        :DO (PRINT X))
  (1 10))
 ((1 10) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :IN (LIST A) :BY FN
        :DO (PRINT X))
  (8))
 ((1 14) (8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :IN (LIST A) :BY FN
        :DO (PRINT X))
  (6))
 ((1 10) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :IN (LIST A) :BY FN
        :DO (PRINT X))
  (2))
 ((12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :ON (LIST A) :BY FN
        :DO (PRINT X))
  (1 14))
 ((1 14) (8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :ON (LIST A) :BY FN
        :DO (PRINT X))
  (12))
 ((12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :ON (LIST A) :BY FN
        :DO (PRINT X))
  (1 10))
 ((1 10) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :ON (LIST A) :BY FN
        :DO (PRINT X))
  (8))
 ((1 14) (8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FN := #'CDDR
        :AND A
        :FOR X :ON (LIST A) :BY FN
        :DO (PRINT X))
  (2))
 ((12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO := NIL
        :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (1 10))
 ((1 10) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO := NIL
        :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (6))
 ((1 10) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO := NIL
        :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (2))
 ((2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO
        :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (1 8))
 ((1 8) (4)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO
        :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (4))
 ((1 8) (4)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :WITH FOO
        :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (2))
 ((2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (1 6))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :ON '(1 2 3)
        :DO (PRINT X))
  (2))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (F X)
        :DO (F X))
  (1 8))
 ((1 8) (1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (F X)
        :DO (F X))
  (1 6))
 ((1 8) (1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (F X)
        :DO (F X))
  (2))
 ((1 8) (1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1
        :DO (F X))
  (1 6))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1
        :DO (F X))
  (2))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :ACROSS '(1 2 3)
        :DO (F X))
  (1 6))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :ACROSS '(1 2 3)
        :DO (F X))
  (2))
 ((1 6) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR (X . Y) := (F)
        :DO (F X Y))
  (2 6))
 ((2 6) (2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR (X . Y) := (F)
        :DO (F X Y))
  (1 6))
 ((1 6) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR (X . Y) := (F)
        :DO (F X Y))
  (2 2))
 ((2 6) (2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR (X . Y) := (F)
        :DO (F X Y))
  (0 2))
 ((1 6) (0 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE V)
        DO (PRINT (CONS K V)))
  (2 1 11))
 ((2 1 11) (1 9)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE V)
        DO (PRINT (CONS K V)))
  (1 1 11))
 ((1 1 11) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE V)
        DO (PRINT (CONS K V)))
  (1 9))
 ((2 1 11) (1 9)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE V)
        DO (PRINT (CONS K V)))
  (2))
 ((1 1 11) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2))
        DO (PRINT K))
  (1 9))
 ((1 9) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2))
        DO (PRINT K))
  (2))
 ((1 9) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((V 0))
    (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE
                                                                                  V)
          DO (PRINT (CONS K V)))
    V)
  (0 0 1))
 ((3) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((V 0))
    (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE
                                                                                  V)
          DO (PRINT (CONS K V)))
    V)
  (3))
 ((3) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((V 0))
    (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE
                                                                                  V)
          DO (PRINT (CONS K V)))
    V)
  (2 1 11 2))
 ((2 1 11 2) (1 9 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((V 0))
    (LOOP FOR K BEING EACH HASH-KEY IN (PLIST-HASH-TABLE '((:A) 1 (:B) 2)) USING (HASH-VALUE
                                                                                  V)
          DO (PRINT (CONS K V)))
    V)
  (1 9 2))
 ((2 1 11 2) (1 9 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR V BEING EACH HASH-VALUES OF *HT* USING (HASH-KEY K)
        DO (FORMAT T "~a=>~a~%" K V))
  (4 11))
 ((4 11) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR V BEING EACH HASH-VALUES OF *HT* USING (HASH-KEY K)
        DO (FORMAT T "~a=>~a~%" K V))
  (3 11))
 ((3 11) (1 9)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR V BEING EACH HASH-VALUES OF *HT* USING (HASH-KEY K)
        DO (FORMAT T "~a=>~a~%" K V))
  (1 9))
 ((3 11) (1 9)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR V BEING EACH HASH-VALUES OF *HT* USING (HASH-KEY K)
        DO (FORMAT T "~a=>~a~%" K V))
  (2))
 ((4 11) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR V BEING THE HASH-VALUE IN *HT*
        DO (PRINT V))
  (1 9))
 ((1 9) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP FOR V BEING THE HASH-VALUE IN *HT*
        DO (PRINT V))
  (2))
 ((1 9) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((PACKAGE-NAME (F)))
    (LOOP :FOR NAME :BEING :EACH :EXTERNAL-SYMBOL :IN PACKAGE-NAME
          :DO (PRINT NAME)))
  (1 9 2))
 ((1 9 2) (2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((PACKAGE-NAME (F)))
    (LOOP :FOR NAME :BEING :EACH :EXTERNAL-SYMBOL :IN PACKAGE-NAME
          :DO (PRINT NAME)))
  (7 2))
 ((7 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((PACKAGE-NAME (F)))
    (LOOP :FOR NAME :BEING :EACH :EXTERNAL-SYMBOL :IN PACKAGE-NAME
          :DO (PRINT NAME)))
  (2 2))
 ((1 9 2) (2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((PACKAGE-NAME (F)))
    (LOOP :FOR NAME :BEING :EACH :EXTERNAL-SYMBOL :IN PACKAGE-NAME
          :DO (PRINT NAME)))
  (0 0 1))
 ((7 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR NAME :BEING :EACH :EXTERNAL-SYMBOL
        :DO (PRINT NAME))
  (1 7))
 ((1 7) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR NAME :BEING :EACH :EXTERNAL-SYMBOL
        :DO (PRINT NAME))
  (2))
 ((1 7) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :IN '(1 2 3) :BY Y
        :AND Y := #'CDDR
        :DO (PRINT X))
  (1 12))
 ((1 12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :IN '(1 2 3) :BY Y
        :AND Y := #'CDDR
        :DO (PRINT X))
  (8))
 ((8) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :IN '(1 2 3) :BY Y
        :AND Y := #'CDDR
        :DO (PRINT X))
  (6))
 ((8) (6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X :IN '(1 2 3) :BY Y
        :AND Y := #'CDDR
        :DO (PRINT X))
  (2))
 ((1 12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := 2 :THEN (+ X 1)
        :DO (CONS X Y))
  (2 14))
 ((2 14) (8) (1 6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := 2 :THEN (+ X 1)
        :DO (CONS X Y))
  (1 14))
 ((1 14) (1 12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := 2 :THEN (+ X 1)
        :DO (CONS X Y))
  (1 12))
 ((1 14) (1 12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := 2 :THEN (+ X 1)
        :DO (CONS X Y))
  (8))
 ((2 14) (8) (1 6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := 2 :THEN (+ X 1)
        :DO (CONS X Y))
  (1 6))
 ((2 14) (8) (1 6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := 2 :THEN (+ X 1)
        :DO (CONS X Y))
  (2))
 ((1 14) (1 12) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := X :THEN (+ X 1))
  (1 12))
 ((1 12) (10) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := X :THEN (+ X 1))
  (8))
 ((8) (1 6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := X :THEN (+ X 1))
  (1 6))
 ((8) (1 6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LOOP :FOR X := 1 :THEN (+ Y 1)
        :AND Y := X :THEN (+ X 1))
  (2))
 ((1 12) (10) (2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 8 10))
 ((1 8 10) (2 10)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 10))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 10))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 10))
 ((1 8 10) (2 10)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 8 9))
 ((1 8 9) (2 9)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 9))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 9))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 9))
 ((1 8 9) (2 9)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 8 8))
 ((1 8 8) (2 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 8))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 8))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 8))
 ((1 8 8) (2 8)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 8 7))
 ((1 8 7) (2 7)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 7))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 7))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 7))
 ((1 8 7) (2 7)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 10 6))
 ((1 10 6) (2 6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (8 6))
 ((8 6) (0 2 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 6))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 6))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 6))
 ((1 10 6) (2 6)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 8 5))
 ((1 8 5) (2 5)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 5))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 5))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 5))
 ((1 8 5) (2 5)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 8 4))
 ((1 8 4) (2 4)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 4))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 4))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 4))
 ((1 8 4) (2 4)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 6 3))
 ((1 6 3) (2 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 3))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 3))
 ((1 6 3) (2 3)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (1 8 2))
 ((1 8 2) (2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (6 2))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (4 2))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 2))
 ((1 8 2) (2 2)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (0 2 1))
 ((8 6) (0 2 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (0 1 1))
 ((6 10) (6 9) (6 8) (6 7) (6 6) (6 5) (6 4) (6 2) (0 1 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (0 0 1))
 ((4 10) (4 9) (4 8) (4 7) (4 6) (4 5) (4 4) (4 3) (4 2) (0 0 1)))
((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
  (LET ((START (F)) (END (G)) (STEP (H)))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :TO
          END :BY STEP
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :ABOVE
          END
          :DO (PRINT X))
    (LOOP :FOR X :DOWNFROM START :TO
          END
          :DO (PRINT X))
    (LOOP :FOR X :FROM START :DOWNTO
          END
          :DO (PRINT X)))
  (2 2))
 ((1 8 2) (2 2)))
