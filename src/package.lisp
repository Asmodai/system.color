;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; package.lisp --- Color system package.
;;;
;;; Time-stamp: <Saturday Jan  2, 2016 21:20:11 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 00:13:37
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
(in-package #:cl-user)

(defpackage #:system.color
  (:use #:common-lisp
        #:core.zetalisp
        #:core.clos
        #:core.mop
        #:core.excl)
  (:shadowing-import-from #:core.mop #:standard-method
                                     #:standard-generic-function
                                     #:defmethod
                                     #:defgeneric
                                     #:standard-class)
  (:export #:rgb->hsl
           #:hsl->rgb
           #:rgb->ihs
           #:ihs->rgb
           #:rgb->intensity/average
           #:rgb->intensity/lightness
           #:rgb->intensity/luminosity
           #:color-clamp
           #:rgb-float->byte
           #:rgb-byte->float
           #:+foreground+
           #:+background+
           #:+flipping-ink+
           #:+opaque+
           #:+transparent+
           #:color
           #:color-red-value
           #:color-green-value
           #:color-blue-value
           #:color-alpha-value
           #:make-color-rgb
           #:make-color-grayscale
           #:color-rgb
           #:color-argb
           #:color-rgba
           #:color-grayscale
           #:color-ihs
           #:color-luminosity
           #:color-intensity
           #:color-almost-white-p
           #:color-invert
           #:color->hexcode
           #:color->rgb/w3c
           #:color->rgba/w3c
           #:+transparent-color+
           #:+opaque-color+
           #:+white+
           #:+black+
           #:+red+
           #:+green+
           #:+blue+
           #:+magenta+
           #:+yellow+
           #:+dim-gray+
           #:+gray+
           #:+light-gray+
           #:rgb-pixel
           #:grayscale-pixel
           #:pixel
           #:color->argb/pixel
           #:color->rgba/pixel
           #:color->grayscale/pixel
           #:argb/pixel->color
           #:rgba/pixel->color
           #:grayscale/pixel->color
           #:color-complement
           #:color-gradient
           #:color-saturate
           #:color-desaturate
           #:color-lighten
           #:color-darken
           #:color-distance
           #:equal-p
           #:nequal-p
           #:color-blend
           #:add
           #:subtract
           #:color-triad
           #:color-tetriad
           ))

;;; package.lisp ends here
