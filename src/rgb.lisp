;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.COLOR; Base: 10; Lowercase: Yes -*-
;;;
;;; rgb.lisp --- X11 RGB values.
;;;
;;; Time-stamp: <Saturday Nov 21, 2015 08:29:42 asmodai>
;;; Revision:   6
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 07:20:53
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

;; (defvar +color+ (make-color-rgb x y z)
(defmacro define-color (&body options)
  "Define a colour with the given options.

:RGB name red green blue
  generate a colour with the given name and RGB components.
"
  (ecase (first options)
    (:rgb
     (let ((cname (intern (concatenate
                           'string
                           "+"
                           (string-upcase (string (second options)))
                           "+")))
           (red (third options))
           (green (fourth options))
           (blue (fifth options)))
       `(progn (defvar ,cname (make-color-rgb ,red ,green ,blue))
               (export ',cname))))))

(define-color :rgb snow 255 250 250)
(define-color :rgb ghostwhite 248 248 255)
(define-color :rgb whitesmoke 245 245 245)
(define-color :rgb gainsboro 220 220 220)
(define-color :rgb floralwhite 255 250 240)
(define-color :rgb oldlace 253 245 230)
(define-color :rgb linen 250 240 230)
(define-color :rgb antiquewhite 250 235 215)
(define-color :rgb papayawhip 255 239 213)
(define-color :rgb blanchedalmond 255 235 205)
(define-color :rgb bisque 255 228 196)
(define-color :rgb peachpuff 255 218 185)
(define-color :rgb navajowhite 255 222 173)
(define-color :rgb moccasin 255 228 181)
(define-color :rgb cornsilk 255 248 220)
(define-color :rgb ivory 255 255 240)
(define-color :rgb lemonchiffon 255 250 205)
(define-color :rgb seashell 255 245 238)
(define-color :rgb honeydew 240 255 240)
(define-color :rgb mintcream 245 255 250)
(define-color :rgb azure 240 255 255)
(define-color :rgb aliceblue 240 248 255)
(define-color :rgb lavender 230 230 250)
(define-color :rgb lavenderblush 255 240 245)
(define-color :rgb mistyrose 255 228 225)
(define-color :rgb white 255 255 255)
(define-color :rgb black 0 0 0)
(define-color :rgb darkslategray 47 79 79)
(define-color :rgb darkslategrey 47 79 79)
(define-color :rgb dimgray 105 105 105)
(define-color :rgb dimgrey 105 105 105)
(define-color :rgb slategray 112 128 144)
(define-color :rgb slategrey 112 128 144)
(define-color :rgb lightslategray 119 136 153)
(define-color :rgb lightslategrey 119 136 153)
(define-color :rgb gray 190 190 190)
(define-color :rgb grey 190 190 190)
(define-color :rgb lightgrey 211 211 211)
(define-color :rgb lightgray 211 211 211)
(define-color :rgb midnightblue 25 25 112)
(define-color :rgb navy 0 0 128)
(define-color :rgb navyblue 0 0 128)
(define-color :rgb cornflowerblue 100 149 237)
(define-color :rgb darkslateblue 72 61 139)
(define-color :rgb slateblue 106 90 205)
(define-color :rgb mediumslateblue 123 104 238)
(define-color :rgb lightslateblue 132 112 255)
(define-color :rgb mediumblue 0 0 205)
(define-color :rgb royalblue 65 105 225)
(define-color :rgb blue 0 0 255)
(define-color :rgb dodgerblue 30 144 255)
(define-color :rgb deepskyblue 0 191 255)
(define-color :rgb skyblue 135 206 235)
(define-color :rgb lightskyblue 135 206 250)
(define-color :rgb steelblue 70 130 180)
(define-color :rgb lightsteelblue 176 196 222)
(define-color :rgb lightblue 173 216 230)
(define-color :rgb powderblue 176 224 230)
(define-color :rgb paleturquoise 175 238 238)
(define-color :rgb darkturquoise 0 206 209)
(define-color :rgb mediumturquoise 72 209 204)
(define-color :rgb turquoise 64 224 208)
(define-color :rgb cyan 0 255 255)
(define-color :rgb lightcyan 224 255 255)
(define-color :rgb cadetblue 95 158 160)
(define-color :rgb mediumaquamarine 102 205 170)
(define-color :rgb aquamarine 127 255 212)
(define-color :rgb darkgreen 0 100 0)
(define-color :rgb darkolivegreen 85 107 47)
(define-color :rgb darkseagreen 143 188 143)
(define-color :rgb seagreen 46 139 87)
(define-color :rgb mediumseagreen 60 179 113)
(define-color :rgb lightseagreen 32 178 170)
(define-color :rgb palegreen 152 251 152)
(define-color :rgb springgreen 0 255 127)
(define-color :rgb lawngreen 124 252 0)
(define-color :rgb green 0 255 0)
(define-color :rgb chartreuse 127 255 0)
(define-color :rgb mediumspringgreen 0 250 154)
(define-color :rgb greenyellow 173 255 47)
(define-color :rgb limegreen 50 205 50)
(define-color :rgb yellowgreen 154 205 50)
(define-color :rgb forestgreen 34 139 34)
(define-color :rgb olivedrab 107 142 35)
(define-color :rgb darkkhaki 189 183 107)
(define-color :rgb khaki 240 230 140)
(define-color :rgb palegoldenrod 238 232 170)
(define-color :rgb lightgoldenrodyellow 250 250 210)
(define-color :rgb lightyellow 255 255 224)
(define-color :rgb yellow 255 255 0)
(define-color :rgb gold 255 215 0)
(define-color :rgb lightgoldenrod 238 221 130)
(define-color :rgb goldenrod 218 165 32)
(define-color :rgb darkgoldenrod 184 134 11)
(define-color :rgb rosybrown 188 143 143)
(define-color :rgb indianred 205 92 92)
(define-color :rgb saddlebrown 139 69 19)
(define-color :rgb sienna 160 82 45)
(define-color :rgb peru 205 133 63)
(define-color :rgb burlywood 222 184 135)
(define-color :rgb beige 245 245 220)
(define-color :rgb wheat 245 222 179)
(define-color :rgb sandybrown 244 164 96)
(define-color :rgb tan 210 180 140)
(define-color :rgb chocolate 210 105 30)
(define-color :rgb firebrick 178 34 34)
(define-color :rgb brown 165 42 42)
(define-color :rgb darksalmon 233 150 122)
(define-color :rgb salmon 250 128 114)
(define-color :rgb lightsalmon 255 160 122)
(define-color :rgb orange 255 165 0)
(define-color :rgb darkorange 255 140 0)
(define-color :rgb coral 255 127 80)
(define-color :rgb lightcoral 240 128 128)
(define-color :rgb tomato 255 99 71)
(define-color :rgb orangered 255 69 0)
(define-color :rgb red 255 0 0)
(define-color :rgb hotpink 255 105 180)
(define-color :rgb deeppink 255 20 147)
(define-color :rgb pink 255 192 203)
(define-color :rgb lightpink 255 182 193)
(define-color :rgb palevioletred 219 112 147)
(define-color :rgb maroon 176 48 96)
(define-color :rgb mediumvioletred 199 21 133)
(define-color :rgb violetred 208 32 144)
(define-color :rgb magenta 255 0 255)
(define-color :rgb violet 238 130 238)
(define-color :rgb plum 221 160 221)
(define-color :rgb orchid 218 112 214)
(define-color :rgb mediumorchid 186 85 211)
(define-color :rgb darkorchid 153 50 204)
(define-color :rgb darkviolet 148 0 211)
(define-color :rgb blueviolet 138 43 226)
(define-color :rgb purple 160 32 240)
(define-color :rgb mediumpurple 147 112 219)
(define-color :rgb thistle 216 191 216)
(define-color :rgb snow1 255 250 250)
(define-color :rgb snow2 238 233 233)
(define-color :rgb snow3 205 201 201)
(define-color :rgb snow4 139 137 137)
(define-color :rgb seashell1 255 245 238)
(define-color :rgb seashell2 238 229 222)
(define-color :rgb seashell3 205 197 191)
(define-color :rgb seashell4 139 134 130)
(define-color :rgb AntiqueWhite1 255 239 219)
(define-color :rgb AntiqueWhite2 238 223 204)
(define-color :rgb AntiqueWhite3 205 192 176)
(define-color :rgb AntiqueWhite4 139 131 120)
(define-color :rgb bisque1 255 228 196)
(define-color :rgb bisque2 238 213 183)
(define-color :rgb bisque3 205 183 158)
(define-color :rgb bisque4 139 125 107)
(define-color :rgb PeachPuff1 255 218 185)
(define-color :rgb PeachPuff2 238 203 173)
(define-color :rgb PeachPuff3 205 175 149)
(define-color :rgb PeachPuff4 139 119 101)
(define-color :rgb NavajoWhite1 255 222 173)
(define-color :rgb NavajoWhite2 238 207 161)
(define-color :rgb NavajoWhite3 205 179 139)
(define-color :rgb NavajoWhite4 139 121 94)
(define-color :rgb LemonChiffon1 255 250 205)
(define-color :rgb LemonChiffon2 238 233 191)
(define-color :rgb LemonChiffon3 205 201 165)
(define-color :rgb LemonChiffon4 139 137 112)
(define-color :rgb cornsilk1 255 248 220)
(define-color :rgb cornsilk2 238 232 205)
(define-color :rgb cornsilk3 205 200 177)
(define-color :rgb cornsilk4 139 136 120)
(define-color :rgb ivory1 255 255 240)
(define-color :rgb ivory2 238 238 224)
(define-color :rgb ivory3 205 205 193)
(define-color :rgb ivory4 139 139 131)
(define-color :rgb honeydew1 240 255 240)
(define-color :rgb honeydew2 224 238 224)
(define-color :rgb honeydew3 193 205 193)
(define-color :rgb honeydew4 131 139 131)
(define-color :rgb LavenderBlush1 255 240 245)
(define-color :rgb LavenderBlush2 238 224 229)
(define-color :rgb LavenderBlush3 205 193 197)
(define-color :rgb LavenderBlush4 139 131 134)
(define-color :rgb MistyRose1 255 228 225)
(define-color :rgb MistyRose2 238 213 210)
(define-color :rgb MistyRose3 205 183 181)
(define-color :rgb MistyRose4 139 125 123)
(define-color :rgb azure1 240 255 255)
(define-color :rgb azure2 224 238 238)
(define-color :rgb azure3 193 205 205)
(define-color :rgb azure4 131 139 139)
(define-color :rgb SlateBlue1 131 111 255)
(define-color :rgb SlateBlue2 122 103 238)
(define-color :rgb SlateBlue3 105 89 205)
(define-color :rgb SlateBlue4 71 60 139)
(define-color :rgb RoyalBlue1 72 118 255)
(define-color :rgb RoyalBlue2 67 110 238)
(define-color :rgb RoyalBlue3 58 95 205)
(define-color :rgb RoyalBlue4 39 64 139)
(define-color :rgb blue1 0 0 255)
(define-color :rgb blue2 0 0 238)
(define-color :rgb blue3 0 0 205)
(define-color :rgb blue4 0 0 139)
(define-color :rgb DodgerBlue1 30 144 255)
(define-color :rgb DodgerBlue2 28 134 238)
(define-color :rgb DodgerBlue3 24 116 205)
(define-color :rgb DodgerBlue4 16 78 139)
(define-color :rgb SteelBlue1 99 184 255)
(define-color :rgb SteelBlue2 92 172 238)
(define-color :rgb SteelBlue3 79 148 205)
(define-color :rgb SteelBlue4 54 100 139)
(define-color :rgb DeepSkyBlue1 0 191 255)
(define-color :rgb DeepSkyBlue2 0 178 238)
(define-color :rgb DeepSkyBlue3 0 154 205)
(define-color :rgb DeepSkyBlue4 0 104 139)
(define-color :rgb SkyBlue1 135 206 255)
(define-color :rgb SkyBlue2 126 192 238)
(define-color :rgb SkyBlue3 108 166 205)
(define-color :rgb SkyBlue4 74 112 139)
(define-color :rgb LightSkyBlue1 176 226 255)
(define-color :rgb LightSkyBlue2 164 211 238)
(define-color :rgb LightSkyBlue3 141 182 205)
(define-color :rgb LightSkyBlue4 96 123 139)
(define-color :rgb SlateGray1 198 226 255)
(define-color :rgb SlateGray2 185 211 238)
(define-color :rgb SlateGray3 159 182 205)
(define-color :rgb SlateGray4 108 123 139)
(define-color :rgb LightSteelBlue1 202 225 255)
(define-color :rgb LightSteelBlue2 188 210 238)
(define-color :rgb LightSteelBlue3 162 181 205)
(define-color :rgb LightSteelBlue4 110 123 139)
(define-color :rgb LightBlue1 191 239 255)
(define-color :rgb LightBlue2 178 223 238)
(define-color :rgb LightBlue3 154 192 205)
(define-color :rgb LightBlue4 104 131 139)
(define-color :rgb LightCyan1 224 255 255)
(define-color :rgb LightCyan2 209 238 238)
(define-color :rgb LightCyan3 180 205 205)
(define-color :rgb LightCyan4 122 139 139)
(define-color :rgb PaleTurquoise1 187 255 255)
(define-color :rgb PaleTurquoise2 174 238 238)
(define-color :rgb PaleTurquoise3 150 205 205)
(define-color :rgb PaleTurquoise4 102 139 139)
(define-color :rgb CadetBlue1 152 245 255)
(define-color :rgb CadetBlue2 142 229 238)
(define-color :rgb CadetBlue3 122 197 205)
(define-color :rgb CadetBlue4 83 134 139)
(define-color :rgb turquoise1 0 245 255)
(define-color :rgb turquoise2 0 229 238)
(define-color :rgb turquoise3 0 197 205)
(define-color :rgb turquoise4 0 134 139)
(define-color :rgb cyan1 0 255 255)
(define-color :rgb cyan2 0 238 238)
(define-color :rgb cyan3 0 205 205)
(define-color :rgb cyan4 0 139 139)
(define-color :rgb DarkSlateGray1 151 255 255)
(define-color :rgb DarkSlateGray2 141 238 238)
(define-color :rgb DarkSlateGray3 121 205 205)
(define-color :rgb DarkSlateGray4 82 139 139)
(define-color :rgb aquamarine1 127 255 212)
(define-color :rgb aquamarine2 118 238 198)
(define-color :rgb aquamarine3 102 205 170)
(define-color :rgb aquamarine4 69 139 116)
(define-color :rgb DarkSeaGreen1 193 255 193)
(define-color :rgb DarkSeaGreen2 180 238 180)
(define-color :rgb DarkSeaGreen3 155 205 155)
(define-color :rgb DarkSeaGreen4 105 139 105)
(define-color :rgb SeaGreen1 84 255 159)
(define-color :rgb SeaGreen2 78 238 148)
(define-color :rgb SeaGreen3 67 205 128)
(define-color :rgb SeaGreen4 46 139 87)
(define-color :rgb PaleGreen1 154 255 154)
(define-color :rgb PaleGreen2 144 238 144)
(define-color :rgb PaleGreen3 124 205 124)
(define-color :rgb PaleGreen4 84 139 84)
(define-color :rgb SpringGreen1 0 255 127)
(define-color :rgb SpringGreen2 0 238 118)
(define-color :rgb SpringGreen3 0 205 102)
(define-color :rgb SpringGreen4 0 139 69)
(define-color :rgb green1 0 255 0)
(define-color :rgb green2 0 238 0)
(define-color :rgb green3 0 205 0)
(define-color :rgb green4 0 139 0)
(define-color :rgb chartreuse1 127 255 0)
(define-color :rgb chartreuse2 118 238 0)
(define-color :rgb chartreuse3 102 205 0)
(define-color :rgb chartreuse4 69 139 0)
(define-color :rgb OliveDrab1 192 255 62)
(define-color :rgb OliveDrab2 179 238 58)
(define-color :rgb OliveDrab3 154 205 50)
(define-color :rgb OliveDrab4 105 139 34)
(define-color :rgb DarkOliveGreen1 202 255 112)
(define-color :rgb DarkOliveGreen2 188 238 104)
(define-color :rgb DarkOliveGreen3 162 205 90)
(define-color :rgb DarkOliveGreen4 110 139 61)
(define-color :rgb khaki1 255 246 143)
(define-color :rgb khaki2 238 230 133)
(define-color :rgb khaki3 205 198 115)
(define-color :rgb khaki4 139 134 78)
(define-color :rgb LightGoldenrod1 255 236 139)
(define-color :rgb LightGoldenrod2 238 220 130)
(define-color :rgb LightGoldenrod3 205 190 112)
(define-color :rgb LightGoldenrod4 139 129 76)
(define-color :rgb LightYellow1 255 255 224)
(define-color :rgb LightYellow2 238 238 209)
(define-color :rgb LightYellow3 205 205 180)
(define-color :rgb LightYellow4 139 139 122)
(define-color :rgb yellow1 255 255 0)
(define-color :rgb yellow2 238 238 0)
(define-color :rgb yellow3 205 205 0)
(define-color :rgb yellow4 139 139 0)
(define-color :rgb gold1 255 215 0)
(define-color :rgb gold2 238 201 0)
(define-color :rgb gold3 205 173 0)
(define-color :rgb gold4 139 117 0)
(define-color :rgb goldenrod1 255 193 37)
(define-color :rgb goldenrod2 238 180 34)
(define-color :rgb goldenrod3 205 155 29)
(define-color :rgb goldenrod4 139 105 20)
(define-color :rgb DarkGoldenrod1 255 185 15)
(define-color :rgb DarkGoldenrod2 238 173 14)
(define-color :rgb DarkGoldenrod3 205 149 12)
(define-color :rgb DarkGoldenrod4 139 101 8)
(define-color :rgb RosyBrown1 255 193 193)
(define-color :rgb RosyBrown2 238 180 180)
(define-color :rgb RosyBrown3 205 155 155)
(define-color :rgb RosyBrown4 139 105 105)
(define-color :rgb IndianRed1 255 106 106)
(define-color :rgb IndianRed2 238 99 99)
(define-color :rgb IndianRed3 205 85 85)
(define-color :rgb IndianRed4 139 58 58)
(define-color :rgb sienna1 255 130 71)
(define-color :rgb sienna2 238 121 66)
(define-color :rgb sienna3 205 104 57)
(define-color :rgb sienna4 139 71 38)
(define-color :rgb burlywood1 255 211 155)
(define-color :rgb burlywood2 238 197 145)
(define-color :rgb burlywood3 205 170 125)
(define-color :rgb burlywood4 139 115 85)
(define-color :rgb wheat1 255 231 186)
(define-color :rgb wheat2 238 216 174)
(define-color :rgb wheat3 205 186 150)
(define-color :rgb wheat4 139 126 102)
(define-color :rgb tan1 255 165 79)
(define-color :rgb tan2 238 154 73)
(define-color :rgb tan3 205 133 63)
(define-color :rgb tan4 139 90 43)
(define-color :rgb chocolate1 255 127 36)
(define-color :rgb chocolate2 238 118 33)
(define-color :rgb chocolate3 205 102 29)
(define-color :rgb chocolate4 139 69 19)
(define-color :rgb firebrick1 255 48 48)
(define-color :rgb firebrick2 238 44 44)
(define-color :rgb firebrick3 205 38 38)
(define-color :rgb firebrick4 139 26 26)
(define-color :rgb brown1 255 64 64)
(define-color :rgb brown2 238 59 59)
(define-color :rgb brown3 205 51 51)
(define-color :rgb brown4 139 35 35)
(define-color :rgb salmon1 255 140 105)
(define-color :rgb salmon2 238 130 98)
(define-color :rgb salmon3 205 112 84)
(define-color :rgb salmon4 139 76 57)
(define-color :rgb LightSalmon1 255 160 122)
(define-color :rgb LightSalmon2 238 149 114)
(define-color :rgb LightSalmon3 205 129 98)
(define-color :rgb LightSalmon4 139 87 66)
(define-color :rgb orange1 255 165 0)
(define-color :rgb orange2 238 154 0)
(define-color :rgb orange3 205 133 0)
(define-color :rgb orange4 139 90 0)
(define-color :rgb DarkOrange1 255 127 0)
(define-color :rgb DarkOrange2 238 118 0)
(define-color :rgb DarkOrange3 205 102 0)
(define-color :rgb DarkOrange4 139 69 0)
(define-color :rgb coral1 255 114 86)
(define-color :rgb coral2 238 106 80)
(define-color :rgb coral3 205 91 69)
(define-color :rgb coral4 139 62 47)
(define-color :rgb tomato1 255 99 71)
(define-color :rgb tomato2 238 92 66)
(define-color :rgb tomato3 205 79 57)
(define-color :rgb tomato4 139 54 38)
(define-color :rgb OrangeRed1 255 69 0)
(define-color :rgb OrangeRed2 238 64 0)
(define-color :rgb OrangeRed3 205 55 0)
(define-color :rgb OrangeRed4 139 37 0)
(define-color :rgb red1 255 0 0)
(define-color :rgb red2 238 0 0)
(define-color :rgb red3 205 0 0)
(define-color :rgb red4 139 0 0)
(define-color :rgb DeepPink1 255 20 147)
(define-color :rgb DeepPink2 238 18 137)
(define-color :rgb DeepPink3 205 16 118)
(define-color :rgb DeepPink4 139 10 80)
(define-color :rgb HotPink1 255 110 180)
(define-color :rgb HotPink2 238 106 167)
(define-color :rgb HotPink3 205 96 144)
(define-color :rgb HotPink4 139 58 98)
(define-color :rgb pink1 255 181 197)
(define-color :rgb pink2 238 169 184)
(define-color :rgb pink3 205 145 158)
(define-color :rgb pink4 139 99 108)
(define-color :rgb LightPink1 255 174 185)
(define-color :rgb LightPink2 238 162 173)
(define-color :rgb LightPink3 205 140 149)
(define-color :rgb LightPink4 139 95 101)
(define-color :rgb PaleVioletRed1 255 130 171)
(define-color :rgb PaleVioletRed2 238 121 159)
(define-color :rgb PaleVioletRed3 205 104 137)
(define-color :rgb PaleVioletRed4 139 71 93)
(define-color :rgb maroon1 255 52 179)
(define-color :rgb maroon2 238 48 167)
(define-color :rgb maroon3 205 41 144)
(define-color :rgb maroon4 139 28 98)
(define-color :rgb VioletRed1 255 62 150)
(define-color :rgb VioletRed2 238 58 140)
(define-color :rgb VioletRed3 205 50 120)
(define-color :rgb VioletRed4 139 34 82)
(define-color :rgb magenta1 255 0 255)
(define-color :rgb magenta2 238 0 238)
(define-color :rgb magenta3 205 0 205)
(define-color :rgb magenta4 139 0 139)
(define-color :rgb orchid1 255 131 250)
(define-color :rgb orchid2 238 122 233)
(define-color :rgb orchid3 205 105 201)
(define-color :rgb orchid4 139 71 137)
(define-color :rgb plum1 255 187 255)
(define-color :rgb plum2 238 174 238)
(define-color :rgb plum3 205 150 205)
(define-color :rgb plum4 139 102 139)
(define-color :rgb MediumOrchid1 224 102 255)
(define-color :rgb MediumOrchid2 209 95 238)
(define-color :rgb MediumOrchid3 180 82 205)
(define-color :rgb MediumOrchid4 122 55 139)
(define-color :rgb DarkOrchid1 191 62 255)
(define-color :rgb DarkOrchid2 178 58 238)
(define-color :rgb DarkOrchid3 154 50 205)
(define-color :rgb DarkOrchid4 104 34 139)
(define-color :rgb purple1 155 48 255)
(define-color :rgb purple2 145 44 238)
(define-color :rgb purple3 125 38 205)
(define-color :rgb purple4 85 26 139)
(define-color :rgb MediumPurple1 171 130 255)
(define-color :rgb MediumPurple2 159 121 238)
(define-color :rgb MediumPurple3 137 104 205)
(define-color :rgb MediumPurple4 93 71 139)
(define-color :rgb thistle1 255 225 255)
(define-color :rgb thistle2 238 210 238)
(define-color :rgb thistle3 205 181 205)
(define-color :rgb thistle4 139 123 139)
(define-color :rgb gray0 0 0 0)
(define-color :rgb grey0 0 0 0)
(define-color :rgb gray1 3 3 3)
(define-color :rgb grey1 3 3 3)
(define-color :rgb gray2 5 5 5)
(define-color :rgb grey2 5 5 5)
(define-color :rgb gray3 8 8 8)
(define-color :rgb grey3 8 8 8)
(define-color :rgb gray4 10 10 10)
(define-color :rgb grey4 10 10 10)
(define-color :rgb gray5 13 13 13)
(define-color :rgb grey5 13 13 13)
(define-color :rgb gray6 15 15 15)
(define-color :rgb grey6 15 15 15)
(define-color :rgb gray7 18 18 18)
(define-color :rgb grey7 18 18 18)
(define-color :rgb gray8 20 20 20)
(define-color :rgb grey8 20 20 20)
(define-color :rgb gray9 23 23 23)
(define-color :rgb grey9 23 23 23)
(define-color :rgb gray10 26 26 26)
(define-color :rgb grey10 26 26 26)
(define-color :rgb gray11 28 28 28)
(define-color :rgb grey11 28 28 28)
(define-color :rgb gray12 31 31 31)
(define-color :rgb grey12 31 31 31)
(define-color :rgb gray13 33 33 33)
(define-color :rgb grey13 33 33 33)
(define-color :rgb gray14 36 36 36)
(define-color :rgb grey14 36 36 36)
(define-color :rgb gray15 38 38 38)
(define-color :rgb grey15 38 38 38)
(define-color :rgb gray16 41 41 41)
(define-color :rgb grey16 41 41 41)
(define-color :rgb gray17 43 43 43)
(define-color :rgb grey17 43 43 43)
(define-color :rgb gray18 46 46 46)
(define-color :rgb grey18 46 46 46)
(define-color :rgb gray19 48 48 48)
(define-color :rgb grey19 48 48 48)
(define-color :rgb gray20 51 51 51)
(define-color :rgb grey20 51 51 51)
(define-color :rgb gray21 54 54 54)
(define-color :rgb grey21 54 54 54)
(define-color :rgb gray22 56 56 56)
(define-color :rgb grey22 56 56 56)
(define-color :rgb gray23 59 59 59)
(define-color :rgb grey23 59 59 59)
(define-color :rgb gray24 61 61 61)
(define-color :rgb grey24 61 61 61)
(define-color :rgb gray25 64 64 64)
(define-color :rgb grey25 64 64 64)
(define-color :rgb gray26 66 66 66)
(define-color :rgb grey26 66 66 66)
(define-color :rgb gray27 69 69 69)
(define-color :rgb grey27 69 69 69)
(define-color :rgb gray28 71 71 71)
(define-color :rgb grey28 71 71 71)
(define-color :rgb gray29 74 74 74)
(define-color :rgb grey29 74 74 74)
(define-color :rgb gray30 77 77 77)
(define-color :rgb grey30 77 77 77)
(define-color :rgb gray31 79 79 79)
(define-color :rgb grey31 79 79 79)
(define-color :rgb gray32 82 82 82)
(define-color :rgb grey32 82 82 82)
(define-color :rgb gray33 84 84 84)
(define-color :rgb grey33 84 84 84)
(define-color :rgb gray34 87 87 87)
(define-color :rgb grey34 87 87 87)
(define-color :rgb gray35 89 89 89)
(define-color :rgb grey35 89 89 89)
(define-color :rgb gray36 92 92 92)
(define-color :rgb grey36 92 92 92)
(define-color :rgb gray37 94 94 94)
(define-color :rgb grey37 94 94 94)
(define-color :rgb gray38 97 97 97)
(define-color :rgb grey38 97 97 97)
(define-color :rgb gray39 99 99 99)
(define-color :rgb grey39 99 99 99)
(define-color :rgb gray40 102 102 102)
(define-color :rgb grey40 102 102 102)
(define-color :rgb gray41 105 105 105)
(define-color :rgb grey41 105 105 105)
(define-color :rgb gray42 107 107 107)
(define-color :rgb grey42 107 107 107)
(define-color :rgb gray43 110 110 110)
(define-color :rgb grey43 110 110 110)
(define-color :rgb gray44 112 112 112)
(define-color :rgb grey44 112 112 112)
(define-color :rgb gray45 115 115 115)
(define-color :rgb grey45 115 115 115)
(define-color :rgb gray46 117 117 117)
(define-color :rgb grey46 117 117 117)
(define-color :rgb gray47 120 120 120)
(define-color :rgb grey47 120 120 120)
(define-color :rgb gray48 122 122 122)
(define-color :rgb grey48 122 122 122)
(define-color :rgb gray49 125 125 125)
(define-color :rgb grey49 125 125 125)
(define-color :rgb gray50 127 127 127)
(define-color :rgb grey50 127 127 127)
(define-color :rgb gray51 130 130 130)
(define-color :rgb grey51 130 130 130)
(define-color :rgb gray52 133 133 133)
(define-color :rgb grey52 133 133 133)
(define-color :rgb gray53 135 135 135)
(define-color :rgb grey53 135 135 135)
(define-color :rgb gray54 138 138 138)
(define-color :rgb grey54 138 138 138)
(define-color :rgb gray55 140 140 140)
(define-color :rgb grey55 140 140 140)
(define-color :rgb gray56 143 143 143)
(define-color :rgb grey56 143 143 143)
(define-color :rgb gray57 145 145 145)
(define-color :rgb grey57 145 145 145)
(define-color :rgb gray58 148 148 148)
(define-color :rgb grey58 148 148 148)
(define-color :rgb gray59 150 150 150)
(define-color :rgb grey59 150 150 150)
(define-color :rgb gray60 153 153 153)
(define-color :rgb grey60 153 153 153)
(define-color :rgb gray61 156 156 156)
(define-color :rgb grey61 156 156 156)
(define-color :rgb gray62 158 158 158)
(define-color :rgb grey62 158 158 158)
(define-color :rgb gray63 161 161 161)
(define-color :rgb grey63 161 161 161)
(define-color :rgb gray64 163 163 163)
(define-color :rgb grey64 163 163 163)
(define-color :rgb gray65 166 166 166)
(define-color :rgb grey65 166 166 166)
(define-color :rgb gray66 168 168 168)
(define-color :rgb grey66 168 168 168)
(define-color :rgb gray67 171 171 171)
(define-color :rgb grey67 171 171 171)
(define-color :rgb gray68 173 173 173)
(define-color :rgb grey68 173 173 173)
(define-color :rgb gray69 176 176 176)
(define-color :rgb grey69 176 176 176)
(define-color :rgb gray70 179 179 179)
(define-color :rgb grey70 179 179 179)
(define-color :rgb gray71 181 181 181)
(define-color :rgb grey71 181 181 181)
(define-color :rgb gray72 184 184 184)
(define-color :rgb grey72 184 184 184)
(define-color :rgb gray73 186 186 186)
(define-color :rgb grey73 186 186 186)
(define-color :rgb gray74 189 189 189)
(define-color :rgb grey74 189 189 189)
(define-color :rgb gray75 191 191 191)
(define-color :rgb grey75 191 191 191)
(define-color :rgb gray76 194 194 194)
(define-color :rgb grey76 194 194 194)
(define-color :rgb gray77 196 196 196)
(define-color :rgb grey77 196 196 196)
(define-color :rgb gray78 199 199 199)
(define-color :rgb grey78 199 199 199)
(define-color :rgb gray79 201 201 201)
(define-color :rgb grey79 201 201 201)
(define-color :rgb gray80 204 204 204)
(define-color :rgb grey80 204 204 204)
(define-color :rgb gray81 207 207 207)
(define-color :rgb grey81 207 207 207)
(define-color :rgb gray82 209 209 209)
(define-color :rgb grey82 209 209 209)
(define-color :rgb gray83 212 212 212)
(define-color :rgb grey83 212 212 212)
(define-color :rgb gray84 214 214 214)
(define-color :rgb grey84 214 214 214)
(define-color :rgb gray85 217 217 217)
(define-color :rgb grey85 217 217 217)
(define-color :rgb gray86 219 219 219)
(define-color :rgb grey86 219 219 219)
(define-color :rgb gray87 222 222 222)
(define-color :rgb grey87 222 222 222)
(define-color :rgb gray88 224 224 224)
(define-color :rgb grey88 224 224 224)
(define-color :rgb gray89 227 227 227)
(define-color :rgb grey89 227 227 227)
(define-color :rgb gray90 229 229 229)
(define-color :rgb grey90 229 229 229)
(define-color :rgb gray91 232 232 232)
(define-color :rgb grey91 232 232 232)
(define-color :rgb gray92 235 235 235)
(define-color :rgb grey92 235 235 235)
(define-color :rgb gray93 237 237 237)
(define-color :rgb grey93 237 237 237)
(define-color :rgb gray94 240 240 240)
(define-color :rgb grey94 240 240 240)
(define-color :rgb gray95 242 242 242)
(define-color :rgb grey95 242 242 242)
(define-color :rgb gray96 245 245 245)
(define-color :rgb grey96 245 245 245)
(define-color :rgb gray97 247 247 247)
(define-color :rgb grey97 247 247 247)
(define-color :rgb gray98 250 250 250)
(define-color :rgb grey98 250 250 250)
(define-color :rgb gray99 252 252 252)
(define-color :rgb grey99 252 252 252)
(define-color :rgb gray100 255 255 255)
(define-color :rgb grey100 255 255 255)
(define-color :rgb darkgrey 169 169 169)
(define-color :rgb darkgray 169 169 169)
(define-color :rgb darkblue 0 0 139)
(define-color :rgb darkcyan 0 139 139)
(define-color :rgb darkmagenta 139 0 139)
(define-color :rgb darkred 139 0 0)
(define-color :rgb lightgreen 144 238 144)

;; rgb.lisp ends here
