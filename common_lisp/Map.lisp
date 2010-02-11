;;;; Map.lisp
;;;;
;;;;  author: Erik Winkels (aerique@xs4all.nl)
;;;; created: 2010-02-05
;;;; license: Public Domain
;;;;
;;;; You don't need to edit this file, see the MAKE-MOVE function in
;;;; MyTronBot.lisp.

;;; Package

(defpackage :my-tron-bot
  (:use :cl))

(in-package :my-tron-bot)


;;; Parameters

(defparameter *verbose* nil)

(defparameter *input* *standard-input*)
(defparameter *output* *standard-output*)


;;; Classes

(defclass tron-map ()
  ((map :reader map-of :initarg :map :initform nil)
   (position-1 :reader my-position :initform nil)
   (position-2 :reader enemy-position :initform nil)
   (x :reader x-of :initarg :x :initform nil)
   (y :reader y-of :initarg :y :initform nil)))


(defgeneric make-move (map))
(defgeneric parse-map (map))
(defgeneric print-map (map))
(defgeneric set-map-size (map string))
(defgeneric wall? (map x y))

;;; Utility Functions

(defun current-date-time-string ()
  (multiple-value-bind (sec min hou day mon yea)
      (decode-universal-time (get-universal-time))
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            yea mon day hou min sec)))


(let ((log nil))
  (defun logmsg (&rest args)
    (when (and *verbose*
               (not log))
      (setf log (open "sbcl.log" :direction :output :if-exists :append
                                 :if-does-not-exist :create)))
    (when *verbose*
      (format log (with-output-to-string (s) (dolist (a args) (princ a s))))
      (force-output log))))


(defun move (direction)
  (case direction
    (:north (princ "1" *output*))
    (:east  (princ "2" *output*))
    (:south (princ "3" *output*))
    (:west  (princ "4" *output*))
    (:up    (princ "1" *output*))
    (:right (princ "2" *output*))
    (:down  (princ "3" *output*))
    (:left  (princ "4" *output*)))
  (terpri *output*)
  (force-output *output*))


;;; Methods

;; Slow!
(defmethod print-map ((m tron-map))
  (loop repeat (y-of m)
        for y from 0
        do (logmsg "[map] ")
           (loop repeat (x-of m)
                 for x from 0
                 do (logmsg (aref (map-of m) x y)))
           (logmsg "~%")))


(defmethod set-map-size ((m tron-map) (s string))
  (let ((space (position #\space s)))
    (setf (slot-value m 'x) (parse-integer (subseq s 0 space))
          (slot-value m 'y) (parse-integer (subseq s space)))))


(defmethod parse-map ((m tron-map))
  (if (and (x-of m) (y-of m))  ;; do we know the map's size?
      (read-line *input* nil nil)
      (set-map-size m (read-line *input* nil nil)))
  (unless (map-of m)  ;; is the map array initialised?
    (setf (slot-value m 'map)
          (make-array (list (x-of m) (y-of m)))))
  (loop repeat (y-of m)
        for y from 0
        do (loop for c across (read-line *input* nil nil)
                 for x from 0
                 do (setf (aref (map-of m) x y) c)
                    (case c
                      (#\1 (setf (slot-value m 'position-1) (list x y)))
                      (#\2 (setf (slot-value m 'position-2) (list x y)))))))


(defmethod clone ((m tron-map))
  (let ((nm (make-instance 'tron-map)))
    (setf (slot-value nm 'position-1) (my-position m))
    (setf (slot-value nm 'position-2) (enemy-position m))
    (setf (slot-value nm 'x) (x-of m))
    (setf (slot-value nm 'y) (y-of m))
    (setf (slot-value nm 'map) (copy-array (map-of m)))
    nm))



(defmethod wall? ((m tron-map) (x fixnum) (y fixnum))
  (or (< x 0) (>= x (x-of m)) (< y 0) (>= y (y-of m))
      (case (aref (map-of m) x y)
        ((#\# #\1 #\2) t)
        (otherwise nil))))

; (defgeneric clone (map))
; (defgeneric set-wall (map fixnum fixnum))
; (defgeneric move-player (map fixnum fixnum))
; (defgeneric move-enemy (map fixnum fixnum))
; (defgeneric move-both (map fixnum fixnum fixnum fixnum))
; (defgeneric set-wall! (map fixnum fixnum))
; (defgeneric set-player! (map fixnum fixnum))
; (defgeneric set-enemy! (map fixnum fixnum))
; (defgeneric player-position (map))
; 
(defmethod player-position ((m tron-map))
  (my-position m))

(defmethod inside? ((m tron-map) (x fixnum) (y fixnum))
  (and (>= x  0) (< x (x-of m)) (>= y 0) (< y (y-of m))))

(defmethod set-wall! ((m tron-map) (x fixnum) (y fixnum))
  (when (inside? m x y)
    (setf (aref (map-of m) x y) #\#))
  m)

(defmethod set-player! ((m tron-map) (x fixnum) (y fixnum))
  (when (inside? m x y)
    (setf (aref (map-of m) x y) #\1))
  (setf (slot-value m 'position-1) (list x y)))

(defmethod set-enemy! ((m tron-map) (x fixnum) (y fixnum))
  (when (inside? m x y)
    (setf (aref (map-of m) x y) #\2))
  (setf (slot-value m 'position-2) (list x y)))


; this returns a copy 
(defmethod move-player ((m tron-map) (x fixnum) (y fixnum))
  (let* ((nm (clone m))
        (pp (my-position m)))
    (set-wall! nm (car pp) (cadr pp))
    (set-player! nm x y)
    nm))
  
; this returns a copy 
(defmethod move-enemy ((m tron-map) (x fixnum) (y fixnum))
  (let* ((nm (clone m))
        (pp (enemy-position m)))
    (set-wall! nm (car pp) (cadr pp))
    (set-enemy! nm x y)
    nm))

; this returns a copy 
(defmethod move-both ((m tron-map) (x fixnum) (y fixnum) (ex fixnum) (ey fixnum))
  (let* ((nm (clone m))
        (pp (player-position m))
        (ep (enemy-position m)))
    (set-wall! nm (car pp) (cadr pp))
    (set-wall! nm (car ep) (cadr ep))
    (set-player! nm ex ey)
    (set-enemy! nm ex ey)
    nm))

; Dangerous - this just clones and sets the wall
; it does not update player positions
(defmethod set-wall ((m tron-map) (x fixnum) (y fixnum))
  (let ((nm (clone m)))
    (set-wall! nm x y)
    nm))

; fuck common lisp
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))

(defun test-copy-array ()
  (let* ((y (make-array (list 2 2)))
         (x (copy-array y)))
    (setf (aref x 1 1) 1)
    (setf (aref y 0 0) -1)
    (list (aref x 1 1) (aref y 0 0) (aref x 0 0) (aref y 1 1))))


