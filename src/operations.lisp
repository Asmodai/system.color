;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.COLOR; Base: 10; Lowercase: Yes -*-
;;;
;;; operations.lisp --- Color operations.
;;;
;;; Time-stamp: <Saturday Nov 21, 2015 07:16:10 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 00:58:41
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

#-genera
(in-package #:system.color)

(declaim
 #+debug (optimize (speed 0)
                   (safety 3)
                   (debug 3))
 #-debug (optimize (speed 3)
                   (safety 0)
                   (debug 0)))

(defgeneric color-complement (color)
  (:method ((color color))
    (make-color-rgb (color-clamp (- (color-red-value color) 1.0))
                    (color-clamp (- (color-green-value color) 1.0))
                    (color-clamp (- (color-blue-value color) 1.0)))))

(defgeneric color-gradient (start stop steps)
  (:method ((start color) (stop color) steps)
    (let (result)
      (multiple-value-bind (r1 g1 b1)
          (color-rgb start)
        (multiple-value-bind (r2 g2 b2)
            (color-rgb stop)
          (dotimes (step steps)
            (let* ((rs (/ (- r2 r1) (1+ step)))
                   (gs (/ (- g2 g1) (1+ step)))
                   (bs (/ (- b2 b1) (1+ step))))
              (push (list (make-color-rgb (+ r1 rs) (+ g1 gs) (+ b1 bs))) result)))
          (nreverse result))))))
 
(defgeneric color-saturate (color percent)
  (:method ((color color) percent)
    (multiple-value-bind (r g b)
        (color-rgb color)
      (multiple-value-bind (h s l)
          (rgb->hsl r g b)
        (let ((s (color-clamp (+ s (/ percent 100.0)))))
          (multiple-value-bind (r g b)
              (hsl->rgb h s l)
            (make-color-rgb r g b)))))))

(defgeneric color-desaturate (color percent)
  (:method ((color color) percent)
    (color-saturate color (- percent))))

(defgeneric color-lighten (color percent)
  (:method ((color color) percent)
    (multiple-value-bind (r g b)
        (color-rgb color)
      (multiple-value-bind (h s l)
          (rgb->hsl r g b)
        (let ((l (color-clamp (+ l (/ percent 100.0)))))
          (multiple-value-bind (r g b)
              (hsl->rgb h s l)
            (make-color-rgb r g b)))))))

(defgeneric color-darken (color percent)
  (:method ((color color) percent)
    (color-lighten color (- percent))))

;; xxx
(unless (fboundp 'square)
  (defsubst square (x)
    (* x x)))

(defgeneric color-distance (lhs rhs)
  (:method ((lhs color) (rhs color))
    (multiple-value-bind (r1 g1 b1)
        (color-rgb lhs)
      (multiple-value-bind (r2 g2 b2)
          (color-rgb rhs)
        (+ (square (- r1 r2))
           (square (- g1 g2))
           (square (- b1 b2)))))))

(defmethod equal-p ((lhs color) (rhs color))
  (or (eq lhs rhs)
      (multiple-value-bind (lr lg lb)
          (color-rgb lhs)
        (multiple-value-bind (rr rg rb)
            (color-rgb rhs)
          (and (= lr rr)
               (= lg rg)
               (= lb rb))))))

(defmethod nequal-p ((lhs color) (rhs color))
  (not (equal-p lhs rhs)))

(defgeneric color-blend (lhs rhs)
  (:method ((lhs color) (rhs color))
    (multiple-value-bind (r1 g1 b1 a1)
        (color-rgba lhs)
      (multiple-value-bind (r2 g2 b2 a2)
          (color-rgba rhs)
        (make-color-rgb (+ r2 (* (- 1 a2) r1))
                        (+ g2 (* (- 1 a2) g1))
                        (+ b2 (* (- 1 a2) b1))
                        (+ a2 (* (- 1 a2) a1)))))))

(defmethod add ((lhs color) (rhs color))
  (multiple-value-bind (r1 g1 b1)
      (color-rgb lhs)
    (multiple-value-bind (r2 g2 b2)
        (color-rgb rhs)
      (make-color-rgb (color-clamp (/ (+ r1 r2) 2.0))
                      (color-clamp (/ (+ g1 g2) 2.0))
                      (color-clamp (/ (+ b1 b2) 2.0))))))

(defmethod subtract ((lhs color) (rhs color))
  (multiple-value-bind (r1 g1 b1)
      (color-rgb lhs)
    (multiple-value-bind (r2 g2 b2)
        (color-rgb rhs)
      (make-color-rgb (color-clamp (/ (- r1 r2) 2.0))
                      (color-clamp (/ (- g1 g2) 2.0))
                      (color-clamp (/ (- b1 b2) 2.0))))))

(defgeneric color-triad (color)
  (:method ((color color))
    (multiple-value-bind (r g b)
        (color-rgb color)
      (list (make-color-rgb b r g)
            (make-color-rgb g b r)))))

(defgeneric color-tetriad (color)
  (:method ((color color))
    (multiple-value-bind (r g b)
        (color-rgb color)
      (list (make-color-rgb b g r)
            (make-color-rgb g b r)
            (make-color-rgb b r g)))))

;;; operations.lisp ends here
