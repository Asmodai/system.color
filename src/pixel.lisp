;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.COLOR; Base: 10; Lowercase: Yes -*-
;;;
;;; pixel.lisp --- Pixel operations.
;;;
;;; Time-stamp: <Saturday Nov 21, 2015 08:28:45 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 00:08:27
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

(deftype rgb-pixel ()
  '(unsigned-byte 32))

(deftype grayscale-pixel ()
  '(unsigned-byte 16))

(deftype pixel ()
  `(or rgb-pixel grayscale-pixel))

(defgeneric color->argb/pixel (color)
  (:documentation "Convert a colour to an ARGB pixel value.")
  (:method ((color color))
    (with-slots (red-value green-value blue-value alpha-value) color
       (the rgb-pixel (logior (ash (rgb-float->byte alpha-value) 24)
                              (ash (rgb-float->byte red-value) 16)
                              (ash (rgb-float->byte green-value) 8)
                              (rgb-float->byte blue-value))))))

(defgeneric color->rgba/pixel (color)
  (:documentation "Convert a colour to an RGBA pixel value.")
  (:method ((color color))
    (with-slots (red-value green-value blue-value alpha-value) color
       (the rgb-pixel (logior (ash (rgb-float->byte red-value) 24)
                              (ash (rgb-float->byte green-value) 16)
                              (ash (rgb-float->byte blue-value) 8)
                              (rgb-float->byte alpha-value))))))

(defgeneric color->grayscale/pixel (color)
  (:documentation "Converta colour to a grayscale pixel value.")
  (:method ((color color))
    (let ((a (rgb-float->byte (color-alpha-value color)))
          (i (rgb-float->byte (color-intensity color)))) 
      (the grayscale-pixel (logior (ash a 8) i)))))

(defsubst argb/pixel->color (pixel)
  (let ((a (the (unsigned-byte 8) (ldb (byte 8 24) pixel)))
        (r (the (unsigned-byte 8) (ldb (byte 8 16) pixel)))
        (g (the (unsigned-byte 8) (ldb (byte 8 8) pixel)))
        (b (the (unsigned-byte 8) (ldb (byte 8 0) pixel))))
    (make-color-rgb (rgb-byte->float r)
                    (rgb-byte->float g)
                    (rgb-byte->float b)
                    (rgb-byte->float a))))

(define-documentation 'argb/pixel->color 'function
  "Convert an ARGB pixel value into a colour.")

(defsubst rgba/pixel->color (pixel)
  (let ((r (the (unsigned-byte 8) (ldb (byte 8 24) pixel)))
        (g (the (unsigned-byte 8) (ldb (byte 8 16) pixel)))
        (b (the (unsigned-byte 8) (ldb (byte 8 8) pixel)))
        (a (the (unsigned-byte 8) (ldb (byte 8 0) pixel))))
      (make-color-rgb (rgb-byte->float r)
                      (rgb-byte->float g)
                      (rgb-byte->float b)
                      (rgb-byte->float a))))

(define-documentation 'rgba/pixel->color 'function
  "Convert an RGBA pixel value into a colour.")

(defsubst grayscale/pixel->color (pixel)
  (let ((a (the (unsigned-byte 8) (ldb (byte 8 8) pixel)))
        (i (the (unsigned-byte 8) (ldb (byte 8 0) pixel))))
    (make-color-grayscale (rgb-byte->float i)
                     (rgb-byte->float a))))

(define-documentation 'grayscale/pixel->color 'function
  "Convert a grayscale pixel value into a colour.")

;;; pixel.lisp ends here
