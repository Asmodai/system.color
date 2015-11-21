;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; color.asd --- Color system definition.
;;;
;;; Time-stamp: <Saturday Nov 21, 2015 06:22:09 asmodai>
;;; Revision:   1
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 06:18:24
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

#+genera
(error "Please do not load this file into a Symbolics system.
This is only for Common Lisp systems that support ASDF.")

(in-package #:common-lisp-user)

(defpackage system.color.sysdef
  (:use #:asdf
        #:common-lisp))

(in-package #:system.color.sysdef)

(defsystem color
    :name "Color system definition."
    :author "Paul Ward <asmodai@gmail.com>"
    :version "1.0"
    :maintainer "Paul Ward <asmodai@gmail.com>"
    :license "Lisp Lesser General Public License (LLGPL)"
    :description "<fill this in>"
    :long-description "<fill this in>"

    :depends-on (cold)

    :components
    ((:module :src
              :components
              ((:file "package")
               (:file "color-utils" :depends-on ("package"))
               (:file "color" :depends-on ("color-utils"
                                           "package"))
               (:file "operations" :depends-on ("color"
                                                "color-utils"
                                                "package"))
               (:file "pixel" :depends-on ("color"
                                           "color-utils"
                                           "package"))))))

;;; color.asd ends here
