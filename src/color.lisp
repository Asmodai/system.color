;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.COLOR; Base: 10; Lowercase: Yes -*-
;;;
;;; color.lisp --- Color class.
;;;
;;; Time-stamp: <Saturday Nov 21, 2015 07:47:27 asmodai>
;;; Revision:   19
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    19 Nov 2015 11:50:48
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

(defconstant +foreground+ :foreground)
(defconstant +background+ :background)
(defconstant +flipping-ink+ :flipping-ink)

(defvar +opaque+      1.0)
(defvar +transparent+ 0.0)

(defgeneric color-red-value (color))
(defgeneric color-green-value (color))
(defgeneric color-blue-value (color))
(defgeneric color-alpha-value (color))

(defclass color ()
  ((red-value
    :initarg :red
    :initform 0
    :type (or (integer 0 255) (single-float 0.0 1.0)) 
    :accessor color-red-value)
   (green-value
    :initarg :green
    :initform 0
    :type (or (integer 0 255) (single-float 0.0 1.0)) 
    :accessor color-green-value)
   (blue-value
    :initarg :blue
    :initform 0
    :type (or (integer 0 255) (single-float 0.0 1.0)) 
    :accessor color-blue-value)
   (alpha-value
    :initarg :alpha
    :initform +opaque+
    :type (or (integer 0 255) (single-float 0.0 1.0)) 
    :accessor color-alpha-value)))

(defmethod print-object ((object color) stream)
  (print-unreadable-object (object stream
                            :type t
                            :identity t)
    (with-slots (red-value green-value blue-value alpha-value) object
       (format stream "R:~F G:~F B:~F A:~F"
               red-value
               green-value
               blue-value
               alpha-value))))

(defmethod initialize-instance :after ((object color) &key)
  (with-slots (red-value green-value blue-value alpha-value) object
     (setf red-value (rgb-byte->float red-value)
           green-value (rgb-byte->float green-value)
           blue-value (rgb-byte->float blue-value)
           alpha-value (rgb-byte->float alpha-value))))

(defsubst make-color-rgb (red green blue &optional (alpha +opaque+))
  (make-instance 'color
     :red red
     :green green
     :blue blue
     :alpha alpha))

(defsubst make-color-grayscale (intensity &optional (alpha +opaque+))
  (make-instance 'color
     :red intensity
     :green intensity
     :blue intensity
     :alpha alpha))

(defgeneric color-rgb (color)
  (:method ((color color))
    (values (color-red-value color)
            (color-green-value color)
            (color-blue-value color))))

(defgeneric color-argb (color)
  (:method ((color color))
    (values (color-alpha-value color)
            (color-red-value color)
            (color-green-value color)
            (color-blue-value color))))

(defgeneric color-rgba (color)
  (:method ((color color))
    (values (color-red-value color)
            (color-green-value color)
            (color-blue-value color)
            (color-alpha-value color))))

(defgeneric color-intensity (color)
  (:method ((color color))
    (multiple-value-bind (r g b)
        (color-rgb color)
      (rgb->intensity r g b))))

(defgeneric color-grayscale (color)
  (:method ((color color))
    (with-slots (red-value green-value blue-value) color
       (values (rgb->grayscale :luminosity red-value green-value blue-value)
               (color-alpha-value color)))))

(defgeneric color-ihs (color)
  (:method ((color color))
    (multiple-value-bind (r g b)
        (color-rgb color)
      (multiple-value-bind (i h s)
          (rgb->ihs r g b)
        (values i h s)))))

(defgeneric color-luminosity (color)
  (:method ((color color))
    (+ (* 0.299 (color-red-value color))
       (* 0.587 (color-green-value color))
       (* 0.114 (color-blue-value color)))))

(defgeneric color-almost-white-p (color)
  (:method ((color color))
    (< 0.6 (color-luminosity color))))

(defgeneric color-invert (color)
  (:method ((color color))
    (with-slots (red-value green-value blue-value alpha-value) color
       (make-color-rgb (abs (- alpha-value red-value)) 
                       (abs (- alpha-value green-value)) 
                       (abs (- alpha-value blue-value))))))

(defgeneric color->hexcode (color)
  (:method ((color color))
    (with-slots (red-value green-value blue-value) color
       (format nil "#~2,'0X~2,'0X~2,'0X"
               (rgb-float->byte red-value)
               (rgb-float->byte green-value)
               (rgb-float->byte blue-value)))))

(defgeneric color->rgb/w3c (color)
  (:method ((color color))
    (with-slots (red-value green-value blue-value) color
       (format nil "rgb(~D,~D,~D)"
               (rgb-float->byte red-value)
               (rgb-float->byte green-value)
               (rgb-float->byte blue-value)))))

(defgeneric color->rgba/w3c (color)
  (:method ((color color))
    (with-slots (red-value green-value blue-value alpha-value) color
       (format nil "rgba(~D,~D,~D,~F"
               (rgb-float->byte red-value)
               (rgb-float->byte green-value)
               (rgb-float->byte blue-value)
               alpha-value))))

(defvar +transparent-color+ (make-color-rgb 0.0 0.0 0.0 +transparent+))
(defvar +opaque-color+      (make-color-rgb 0.0 0.0 0.0 +opaque+))

;;; color.lisp ends here
