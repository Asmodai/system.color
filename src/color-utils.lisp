;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.COLOR; Base: 10; Lowercase: Yes -*-
;;;
;;; color-utils.lisp --- Some utilities.
;;;
;;; Time-stamp: <Saturday Nov 21, 2015 05:42:01 asmodai>
;;; Revision:   9
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    19 Nov 2015 17:40:15
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

(in-package #:system.color)

(declaim
 #+debug (optimize (speed 0)
                   (safety 3)
                   (debug 3))
 #-debug (optimize (speed 3)
                   (safety 0)
                   (debug 0)))

(defconstant *ihs-rgb-c1* (float (sqrt (/ 1.0 6.0)) 1.0d0))
(defconstant *ihs-rgb-c2* (float (sqrt (/ 1.0 2.0)) 1.0d0))
(defconstant *ihs-rgb-c3* (float (sqrt (/ 1.0 3.0)) 1.0d0))

(defun hue->rgb (v1 v2 h)
  (cond ((< h (/ 6.0))
         (+ v1 (* (- v2 v1) h 6.0)))
        ((< h 0.5)
         v2)
        ((< h (/ 2.0 3))
         (+ v1 (* (- v2 v1) (- (/ 2.0 3) h) 6.0)))
        (t
         v1)))

(defun rgb->hsl (r g b)
  (let* ((max (max r g b))
         (min (min r g b))
         (delta (- max min))
         (l (/ (+ max min) 2.0)))
    (if (= delta 0)
        (values 0.0 0.0 l)
        (let* ((s (if (<= l 0.5)
                      (/ delta (+ max min))
                      (/ delta (- 2.0 max min))))
               (rc (/ (- max r) delta))
               (gc (/ (- max g) delta))
               (bc (/ (- max b) delta))
               (h  (mod (/ (cond
                             ((= r max)
                              (- bc gc))
                             ((= g max)
                              (+ 2.0 rc (- bc)))
                             (t
                              (+ 4.0 gc (- rc))))
                           6.0) 1.0)))
          (values h s l)))))

(defun hsl->rgb (h s l)
  (if (= s 0.0)
      (values l l l)
      (let* ((m2 (if (<= l 0.5)
                     (* l (+ 1.0 s))
                     (- (+ l s) (* l s))))
             (m1 (- (* 2.0 l) m2)))
        (values
         (hue->rgb m1 m2 (mod (+ h (/ 3.0)) 1))
         (hue->rgb m1 m2 h)
         (hue->rgb m1 m2 (mod (- h (/ 3.0)) 1))))))

(defun hsl->rgb (h s l)
  (if (= s 0.0)
      (values l l l)
      (let* ((m2 (if (<= l 0.5)
                     (* l (+ 1.0 s))
                     (- (+ l s) (* l s))))
             (m1 (- (* 2.0 l) m2)))
        (values
         (hue->rgb m1 m2 (mod (+ h (/ 3.0)) 1))
         (hue->rgb m1 m2 h)
         (hue->rgb m1 m2 (mod (- h (/ 3.0)) 1))))))

(defun rgb->ihs (r g b)
  (let* ((red (float r 1.0d0))
         (green (float g 1.0d0))
         (blue (float b 1.0d0))
         (x (* *ihs-rgb-c1* (- (+ red red) blue green)))
         (y (* *ihs-rgb-c2* (- green blue)))
         (z (* *ihs-rgb-c3* (+ red green blue)))
         (q (+ (* x x) (* y y)))
         (i (sqrt (+ q (* z z)))))
    (declare (double-float red green blue x y z q i))
    (if (zerop q)
        (values i 0.0 0.0)
        (let* ((hue (mod (/ (atan y x) 2pi) 1.0))
               (f1 (/ z i))
               (f2 (sqrt (- 1.0 (* f1 f1))))
               (sat (atan f2 f1)))
          (declare (double-float hue f1 f2 sat))
          (values i hue sat)))))

(defun ihs->rgb (i h s)
  (macrolet ((rgb-values (r g b)
               `(values (max 0.0 (min ,r 1.0))
                        (max 0.0 (min ,g 1.0))
                        (max 0.0 (min ,b 1.0)))))
    (let* ((i (float i 1.0d0))
           (h (float h 1.0d0))
           (s (float s 1.0d0))
           (hh (* h 2pi))
           (ss (sin s))
           (x (* *ihs-rgb-c1* ss (cos hh) i))
           (y (* *ihs-rgb-c2* ss (sin hh) i))
           (z (* *ihs-rgb-c3* (cos s) i)))
      (declare (double-float i h s hh ss x y z))
      (rgb-values (+ x x z)
                  (+ y z ( - x))
                  (- z x y)))))

(defsubst rgb->intensity (r g b)
  (sqrt (+ (* r r) (* g g) (* b b))))

(defsubst rgb->grayscale/average (r g b)
  (if (= r g b)
      r
      (/ (+ r g b) 3)))

(defsubst rgb->grayscale/lightness (r g b)
  (if (= r g b)
      r
      (/ (+ (max r g b) (min r g b)) 2)))

(defsubst rgb->grayscale/luminosity (r g b)
  (if (= r g b)
      r
      (+ (* 0.21 r) (* 0.72 g) (* 0.07 b))))

(defselect rgb->grayscale
  ((:average) (r g b)
   (declare (type float r g b))
   (rgb->grayscale/average r g b))
  ((:lightness) (r g b)
   (declare (type float r g b))
   (rgb->grayscale/lightness r g b))
  ((:luminosity) (r g b)
   (declare (type float r g b))
   (rgb->grayscale/luminosity r g b)))

(defsubst color-clamp (value)
  (min 1.0 (max 0.0 value)))

(defsubst rgb-float->byte (value)
  (if (<= value 1.0)
      (let ((v (max 0.0 (min 1.0 value))))
        (floor (if (= v 1.0)
                   255
                   (* v 256.0))))
      value))

(defsubst rgb-byte->float (value)
  (if (> value 1.0)
      (if (= value 255)
          1.0
          (/ value 256.0))
      value))



;;; color-utils.lisp ends here
