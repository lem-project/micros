(defpackage #:micros/walker/tests
  (:use #:cl #:rove))
(in-package #:micros/walker/tests)

(defvar *test-cases*
  '(((MICROS/WALKER:COLLECT-HIGHLIGHT-PATHS
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
     NIL)
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
      (1 2 2))
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
      (6 2))
     NIL)
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
      (5 2))
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
      (1 2 2))
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
     NIL)
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
     ((1 2 0) (0 1 0)))))

(deftest random
  (loop :for (act-form expected) :in *test-cases*
        :do (ok (equal expected (apply (first act-form) (rest act-form)))
                (format nil "~S" act-form))))
