;;;; MyTronBot.lisp
;;;;
;;;;  author: Erik Winkels (aerique@xs4all.nl)
;;;; created: 2010-02-05
;;;; license: Public Domain
;;;;    note: Tested on SBCL 1.0.31.0.debian and 1.0.29 on Windows Vista.
;;;;
;;;; usage: sbcl --script MyTronBot.lisp

(load "Map.lisp")

(in-package :my-tron-bot)


;;; Debugging Switch
;;;
;;; Set this to 't' if you want debugging output written to "sbcl.log" and
;;; for the LOGMSG function to actually do something.
;;;
;;; LOGMSG always appends lines to the log so you can just keep a "tail -f
;;; sbcl.log" running.

(setf *verbose* NIL)  ; Set to NIL when submitting!
;(setf *verbose* t)  ; Set to NIL when submitting!


;;; Functions & Methods

;; MAKE-MOVE is where you come in.  For now it only prints some debugging info
;; and the map to "sbcl.log" if *verbose* is set to T and always moves the
;; player left.
;;
;; The MOVE function announces your move to the tournament engine so this
;; is generally the last thing you do in this function.  Possible moves are:
;; :north, :east, :south, :west, :up, :right, :down and :left.

(defmethod make-move ((m tron-map))
  (logmsg "   my position: " (my-position m) "~%"
          "enemy position: " (enemy-position m) "~%")
  (print-map m)
;  (let ((mov (dof-bot m)))
  (let ((mov (minimax-bot m)))
    (logmsg "my move " mov "~%")
    (move mov)))
;  (move :left))

(defmethod rel-my-position ((m tron-map) xo yo)
  (let* ((mp (my-position m))
         (mx (car mp))
         (my (cadr mp)))
    (list (+ (car mp) xo) (+ (cadr mp) yo))))

(defmethod rel-wall? ((m tron-map) xo yo)
  (let* ((mp (rel-my-position m xo yo)))
    (wall? m (car mp) (cadr mp))))

(defmethod rel-loc-wall? ((m tron-map) (x fixnum) (y fixnum) (xo fixnum) (yo fixnum))
  (wall? m (+ x xo) (+ y yo)))

;(defun degrees-of-freedom-player ((m tron-map)) 
(defun degrees-of-freedom-player (m)
  (+ 
   (if (rel-wall? m 1 0) 0 1)
   (if (rel-wall? m 0 1) 0 1)
   (if (rel-wall? m -1 0) 0 1)
   (if (rel-wall? m 0 -1) 0 1)))

;(defun degrees-of-freedom ((m tron-map) (x fixnum) (y fixnum)) 
(defun degrees-of-freedom (m x y) 
  (+ 
   (if (rel-loc-wall? m x y 1 0) 0 1)
   (if (rel-loc-wall? m x y 0 1) 0 1)
   (if (rel-loc-wall? m x y -1 0) 0 1)
   (if (rel-loc-wall? m x y 0 -1) 0 1)))


(defun filled (m x y) 1)
(defun has-enemy (m x y)
  (let* ((ep (enemy-position m))
         (ex (car ep))
         (ey (cadr ep)))
    (if (and (= ex  x) (= ey y)) 1 0)))


;(defun fill-count! (f (m tron-map) (x fixnum) (y fixnum))  
(defun fill-count! (f m x y)
  (labels ((fc (m xo yo) 
             (let ((nx (+ xo x))
                   (ny (+ yo y)))
               (if (wall? m nx ny) 
                   0
                   (let ((nm (set-wall! m nx ny))) ; not new
                     (+ (funcall f nm nx ny)
                         (fill-count! f nm nx ny)))))))
    (+ 
     (fc m 1 0)
     (fc m 0 1)
     (fc m -1 0)
     (fc m 0 -1))))

;(defun fill-count (f (m tron-map) (x fixnum) (y fixnum))
(defun fill-count (f m x y)
  (fill-count! f (clone m) x y))

(defun max-snd (x o) (if (> (cadr x)  (cadr o)) x o))
(defun min-snd (x o) (if (< (cadr x)  (cadr o)) x o))
(defun choose-max-snd (l)
  (reduce #'max-snd l))
(defun choose (l)
  (nth (random (length l)) l))
(defun random-choose-max-snd (l)
  (let* ((m (choose-max-snd l))
         (mv (cadr m)))
    (choose 
     (remove-if-not (lambda (x) (= (cadr x) mv)) l))))


(defmethod dof-fill ((m tron-map) (x fixnum) (y fixnum))
  (fill-count #'degrees-of-freedom m x y))

(defmethod filled-count ((m tron-map) (x fixnum) (y fixnum))
  (fill-count #'filled m x y))

(defmethod enemy-count ((m tron-map) (x fixnum) (y fixnum))
  (fill-count #'has-enemy m x y))

(defmethod dof-bot ((m tron-map))
  (labels ((idof-fill (m x y) (if (wall? m x y) 0 (dof-fill m x y))))
    (let* ((pp (player-position m))
           (x (car pp))
           (y (cadr pp))
           (l (list
               (list :up (idof-fill m x (- y 1)))
               (list :down (idof-fill m x (+ y 1)))
               (list :left (idof-fill m (- x 1) y))
               (list :right (idof-fill m (+ x 1) y)))))
      (logmsg "L: " l "~%")
      (car 
;       (random-choose-max-snd l)))))
       (choose-max-snd l)))))

(defun surrounded (m x y)
  (and
   (wall? m (+ x 1) y)
   (wall? m (- x 1) y)
   (wall? m x (- y 1))
   (wall? m x (+ y 1))))

(defvar *draw* -5000)
(defvar *win*  10000)
(defvar *lose* -10000)
(defun square (x) (* x x))
(defun score-position (m)
  (let* ((pp (player-position m))
         (ep (enemy-position m))
         (px (car pp))
         (py (cadr pp))
         (ex (car ep))
         (ey (cadr ep))
         ; (pdof (dof-fill m px py))
         ; (edof (dof-fill m ex ey))
         (esurr (surrounded m ex ey)) ; about to die
         (psurr (surrounded m px py)) ; about to die
         (same-zone (> (enemy-count m px py) 0))
         (asize (* (x-of m) (y-of m))))
    (cond
      ((and esurr psurr) *draw*)
      ((and esurr (not psurr)) *win*)
      ((and (not esurr) psurr) *lose*)
      ((not same-zone) 
       (let ((pfill (filled-count m px py))
             (efill (filled-count m ex ey)))
         (* (/ asize 3) (- pfill efill)))) ; how much time do we have left
      (same-zone (- asize (+ (square (- py ey)) (square (- px ex)))))
      (t 0))))

(defun enemy-surrounded (m)
  (let ((ep (enemy-position m)))
    (surrounded m (car ep) (cadr ep))))



(defun wall-list (m x y) 
  (list (list :right (wall? m (+ x 1) y))
        (list :left (wall? m (- x 1) y))
        (list :up (wall? m x (- y 1)))
        (list :down (wall? m x (+ y 1)))))

(defun position-moves (m ep)
  (mapcar #'car 
          (remove-if #'cadr (wall-list m (car ep) (cadr ep)))))

(defun enemy-moves (m)  
  (position-moves m (enemy-position m)))
(defun player-moves (m)  
  (position-moves m (player-position m)))


(defun move-offset (move)
  (case move
    (:right '(1 0))
    (:left '(-1 0))
    (:up   '(0 -1))
    (:down '(0 1))
    (:east '(1 0))
    (:west '(-1 0))
    (:north   '(0 -1))
    (:south '(0 1))))

(defun eval-enemy-move (m move)
  (let* ((moff (move-offset move))
        (ep (enemy-position m))
        (nex (+ (car ep) (car moff)))
        (ney (+ (cadr ep) (cadr moff))))
    (move-enemy m nex ney)))
(defun eval-player-move (m move)
  (let* ((moff (move-offset move))
        (ep (player-position m))
        (nex (+ (car ep) (car moff)))
        (ney (+ (cadr ep) (cadr moff))))
    (move-player m nex ney)))

; list of '(move score)
(defun llast (l)
  (car (last l)))
(defun choose-f-last (f l)
  (reduce (lambda (x o) (if (funcall f (llast x) (llast o)) x o)) l))
(defun choose-min-last (l)
  (choose-f-last (lambda (x o) (< x o)) l))
(defun choose-max-last (l)
  (choose-f-last (lambda (x o) (> x o)) l))

; returns a list of moves and a score
; should be an odd depth
(defun minimax-search (m depth us-or-them)
  (if (= depth 0) 
      (list (score-position m))
      (if us-or-them ; true for them
          (let ((moves (enemy-moves m))) ; them
            (if moves
                (choose-min-last
                 (mapcar (lambda (move) 
                           (let ((nm (eval-enemy-move m move)))
                             (cons move (minimax-search nm (- depth 1) nil))))
                         moves))
                (list (score-position m))))
          (let ((moves (player-moves m))) ; us
            (if moves
                (choose-max-last ; note the max here
                 (mapcar (lambda (move) 
                           (let ((nm (eval-enemy-move m move)))
                             (cons move (minimax-search nm (- depth 1) t))))
                         moves))
                (list (score-position m)))))))

(defmethod minimax-bot ((m tron-map))
  (let ((choice (minimax-search m 5 nil)))
    (logmsg "Choice: " choice "~%")
    (car choice)))


  


;;; Main Program
;;;
;;; You don't need to edit part.  It takes care of communication with the
;;; tournament engine.

(defun main ()
  (logmsg "~&~%=== New Match: " (current-date-time-string) " ===~%")
  (loop with map = (make-instance 'tron-map)
        while (peek-char nil *input* nil nil)
        for move from 0
        do (logmsg "--- move: " move " ---~%")
           (parse-map map)
           (make-move map)))
