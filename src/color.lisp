;;;; color.lisp -- colors and the built-in palettes.

(in-package #:wordperfect-graphics)

(defstruct (color (:constructor make-color (red green blue &optional (alpha 255))))
  "An 8-bit-per-channel color. ALPHA is opacity: 255 is fully opaque.
(WPG2 stores transparency, 0 = opaque; the reader converts.)"
  (red   0 :type (unsigned-byte 8))
  (green 0 :type (unsigned-byte 8))
  (blue  0 :type (unsigned-byte 8))
  (alpha 255 :type (unsigned-byte 8)))

(defun color-hex (color)
  "COLOR as an \"#rrggbb\" string (alpha is not included)."
  (format nil "#~(~2,'0x~2,'0x~2,'0x~)"
          (color-red color) (color-green color) (color-blue color)))

(defun color= (a b)
  (and (= (color-red a) (color-red b))
       (= (color-green a) (color-green b))
       (= (color-blue a) (color-blue b))
       (= (color-alpha a) (color-alpha b))))

;;; The WPG1 default palette is the standard VGA (mode 13h) palette:
;;;   0-15     the 16 EGA colors
;;;   16-31    a 16-step gray ramp
;;;   32-247   nine 24-step hue wheels: three brightnesses x three saturations
;;;   248-255  black
;;; Entries 16-255 are libwpg's values. For 0-15 libwpg uses a Windows-style
;;; set (127/255 levels); this uses the EGA colors WordPerfect for DOS
;;; displayed, as ImageMagick does.

(defparameter +ega-colors+
  '((0 0 0) (0 0 170) (0 170 0) (0 170 170) (170 0 0) (170 0 170) (170 85 0) (170 170 170)
    (85 85 85) (85 85 255) (85 255 85) (85 255 255) (255 85 85) (255 85 255) (255 255 85)
    (255 255 255)))

(defparameter +wpg1-palette-16-255-hex+
  (concatenate 'string
   "0000001414142020202c2c2c383838454545515151616161717171828282929292a2a2a2"
   "b6b6b6cbcbcbe3e3e3ffffff0000ff4100ff7d00ffbe00ffff00ffff00beff007dff0041"
   "ff0000ff4100ff7d00ffbe00ffff00beff007dff0041ff0000ff0000ff4100ff7d00ffbe"
   "00ffff00beff007dff0041ff7d7dff9e7dffbe7dffdf7dffff7dffff7ddfff7dbeff7d9e"
   "ff7d7dff9e7dffbe7dffdf7dffff7ddfff7dbeff7d9eff7d7dff7d7dff9e7dffbe7dffdf"
   "7dffff7ddfff7dbeff7d9effb6b6ffc7b6ffdbb6ffebb6ffffb6ffffb6ebffb6dbffb6c7"
   "ffb6b6ffc7b6ffdbb6ffebb6ffffb6ebffb6dbffb6c7ffb6b6dfb6b6ffc7b6ffdbb6ffeb"
   "b6ffffb6ebffb6dbffb6c7ff0000711c007138007155007171007171005571003871001c"
   "710000711c007138007155007171005571003871001c710000710000711c007138007155"
   "007171005571003871001c71383871453871553871613871713871713861713855713845"
   "713838714538715538716138717138617138557138457138387138387145387155387161"
   "387171386171385571384571515171595171615171695171715171715169715161715159"
   "715151715951716151716951717151697151617151597151517151517159517161517169"
   "517171516971516171515971000041100041200041300041410041410030410020410010"
   "410000411000412000413000414100304100204100104100004100004110004120004130"
   "004141003041002041001041202041282041302041382041412041412038412030412028"
   "412020412820413020413820414120384120304120284120204120204128204130204138"
   "2041412038412030412028413c3c41303c41343c413c3c41413c41413c3c413c34413c30"
   "413c3c41303c41343c413c3c41413c3c413c34413c30413c2c412c2c41302c41342c413c"
   "2c41412c3c412c34412c3041000000000000000000000000000000000000000000000000")
  "Entries 16-255 of the default WPG1 palette, as RRGGBB triples.")

(defun hex-palette (hex start palette)
  "Store the RRGGBB triples in HEX into PALETTE from index START."
  (loop for i from start
        for p from 0 below (length hex) by 6
        do (flet ((channel (k) (parse-integer hex :start (+ p (* 2 k)) :end (+ p (* 2 k) 2)
                                                  :radix 16)))
             (setf (aref palette i) (make-color (channel 0) (channel 1) (channel 2)))))
  palette)

(defun default-wpg1-palette ()
  "A fresh 256-entry vector of COLORs: the palette a WPG1 file starts with."
  (let ((v (make-array 256)))
    (loop for (r g b) in +ega-colors+ for i from 0
          do (setf (aref v i) (make-color r g b)))
    (hex-palette +wpg1-palette-16-255-hex+ 16 v)))

(defparameter +wpg2-default-palette-hex+
  (concatenate 'string
   "000000ffffff7f7f7fbfbfbf00007f007f00007f7f7f00007f007f7f7f000000ff00ff00"
   "00ffffff0000ff00ffffff00000000111111222222333333444444555555666666777777"
   "888888999999aaaaaabbbbbbccccccddddddeeeeeeffffff7b0000910000a70000bd0000"
   "d30000e90000ff0000ff1616ff2c2cff4242ff5858ff6e6eff8484ff9a9affb0b0ffc6c6"
   "7b3d00914800a75300bd5e00d36900e97400ff7f00ff8a16ff952cffa042ffab58ffb66e"
   "ffc184ffcc9affd7b0ffe2c67b7b00919100a7a700bdbd00d3d300e9e900ffff00ffff16"
   "ffff2cffff42ffff58ffff6effff84ffff9affffb0ffffc63d7b0048910053a7005ebd00"
   "69d30074e9007fff008aff1695ff2ca0ff42abff58b6ff6ec1ff84ccff9ad7ffb0e2ffc6"
   "007b0000910000a70000bd0000d30000e90000ff0016ff162cff2c42ff4258ff586eff6e"
   "84ff849aff9ab0ffb0c6ffc6007b3d00914800a75300bd5e00d36900e97400ff7f16ff8a"
   "2cff9542ffa058ffab6effb684ffc19affccb0ffd7c6ffe2007b7b00919100a7a700bdbd"
   "00d3d300e9e900ffff16ffff2cffff42ffff58ffff6effff84ffff9affffb0ffffc6ffff"
   "00567b0064910072a70080b0008ed3009ce400aaff16b1ff2cb8ff42bfff58c6ff6ecdff"
   "84d4ff9adbffb0e2ffc6e9ff002b7b0032910039a70040bd0047d3004ee40055ff1663ff"
   "2c71ff427fff588dff6e9bff84a9ff9ab7ffb0c5ffc6d3ff00007b0000910000a70000bd"
   "0000d30000e40000ff1616ff2c2cff4242ff5858ff6e6eff8484ff9a9affb0b0ffc6c6ff"
   "3d007b4800915300a75e00bd6900d37400e47f00ff8a16ff952cffa042ffab58ffb66eff"
   "c184ffcc9affd7b0ffe2c6ff7b007b910091a700a7bd00bdd300d3e400e4ff00ffff16ff"
   "ff2cffff42ffff58ffff6effff84ffff9affffb0ffffc6ff7b003d910048a70053bd005e"
   "d30069e40074ff007fff168aff2c95ff42a0ff58abff6eb6ff84c1ff9accffb0d7ffc6e2"
   "33291147381761451c734f24875c299c632bb0692bc7d430cc8747d48f57db9c69e3a878"
   "e8b38cf0c49cf7cfb0ffe0c7")
  "The default WPG2 palette, 256 RRGGBB triples (as used by libwpg).")

(defun default-wpg2-palette ()
  "A fresh 256-entry vector of COLORs: the palette a WPG2 file starts with."
  (hex-palette +wpg2-default-palette-hex+ 0 (make-array 256)))

(defun grayscale-palette (depth)
  "A linear black-to-white ramp with 2^DEPTH entries, padded to 256."
  (let* ((n (ash 1 (min depth 8)))
         (v (make-array 256)))
    (dotimes (i 256 v)
      (let ((g (if (and (< i n) (> n 1)) (round (* 255 i) (1- n)) 0)))
        (setf (aref v i) (make-color g g g))))))

(defun copy-palette (palette)
  (map 'vector #'copy-color palette))
