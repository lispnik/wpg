;;;; package.lisp -- the public interface of the WPG reader.

(defpackage #:wordperfect-graphics
  (:use #:cl)
  (:nicknames #:wpg)
  (:export
   ;; Conditions
   #:wpg-error
   #:wpg-format-error
   #:wpg-truncated-error
   #:wpg-truncated-error-position
   #:wpg-truncated-error-wanted
   #:wpg-truncated-error-available
   #:wpg-decode-error
   #:wpg-warning

   ;; Colors and palettes
   #:color
   #:make-color
   #:color-p
   #:color-red
   #:color-green
   #:color-blue
   #:color-alpha
   #:color-hex
   #:color=
   #:default-wpg1-palette
   #:default-wpg2-palette
   #:grayscale-palette

   ;; Reading
   #:read-wpg
   #:read-wpg-header
   #:wpg-file-p

   ;; The file header
   #:wpg-header
   #:wpg-header-p
   #:wpg-header-data-offset
   #:wpg-header-product-type
   #:wpg-header-file-type
   #:wpg-header-major-version
   #:wpg-header-minor-version
   #:wpg-header-encryption-key
   #:wpg-header-reserved

   ;; The parsed image
   #:wpg-image
   #:wpg-image-p
   #:wpg-image-header
   #:wpg-image-version
   #:wpg-image-records
   #:wpg-image-bitmaps
   #:wpg-image-width
   #:wpg-image-height
   #:wpg-image-x-units
   #:wpg-image-y-units
   #:wpg-image-x-offset
   #:wpg-image-y-offset
   #:wpg-image-double-precision
   #:wpg-image-palette
   #:wpg-image-warnings
   #:wpg-image-container
   #:wpg-image-width-inches
   #:wpg-image-height-inches
   #:find-records

   ;; Records
   #:wpg-record
   #:wpg-record-p
   #:wpg-record-index
   #:wpg-record-offset
   #:wpg-record-body-offset
   #:wpg-record-class
   #:wpg-record-type
   #:wpg-record-extension
   #:wpg-record-parent
   #:wpg-record-depth
   #:record-children
   #:wpg-record-length
   #:wpg-record-body
   #:wpg-record-name
   #:wpg-record-fields
   #:wpg-record-error
   #:record-field
   #:record-type-name
   #:record-postscript

   ;; Bitmaps
   #:wpg-bitmap
   #:wpg-bitmap-p
   #:wpg-bitmap-width
   #:wpg-bitmap-height
   #:wpg-bitmap-depth
   #:wpg-bitmap-format
   #:wpg-bitmap-h-resolution
   #:wpg-bitmap-v-resolution
   #:wpg-bitmap-palette
   #:wpg-bitmap-data
   #:wpg-bitmap-row-bytes
   #:wpg-bitmap-x1
   #:wpg-bitmap-y1
   #:wpg-bitmap-x2
   #:wpg-bitmap-y2
   #:wpg-bitmap-rotation
   #:wpg-bitmap-mirror-x
   #:wpg-bitmap-mirror-y
   #:wpg-bitmap-compression
   #:wpg-bitmap-complete-p
   #:wpg-bitmap-record-index
   #:bitmap-color-index
   #:bitmap-pixel
   #:bitmap-rgba-octets
   #:bitmap-rgb-octets
   #:write-bitmap-ppm
   #:*max-bitmap-bytes*
   ;; Defined by the wordperfect-graphics/png system
   #:write-bitmap-png
   #:bitmap-png-octets

   ;; WordPerfect text streams inside WPG files
   #:decode-wp5-text
   #:decode-wp6-text

   ;; Decompression (exposed for tools and tests)
   #:decode-wpg1-rle
   #:decode-wpg2-rle

   ;; The drawing model (scene.lisp)
   #:build-scene
   #:scene #:scene-p #:scene-width #:scene-height #:scene-x-offset #:scene-y-offset
   #:scene-x-units #:scene-y-units #:scene-shapes
   #:style #:style-p #:style-fill #:style-fill-opacity #:style-fill-rule #:style-stroke
   #:style-stroke-opacity #:style-stroke-width #:style-dash
   #:gradient #:gradient-p #:gradient-angle #:gradient-stops #:gradient-direction
   #:gradient-kind #:gradient-center #:gradient-isotropic #:shape-bounds
   #:texture #:texture-p #:texture-bitmap #:texture-width #:texture-height #:texture-color
   #:shape #:shape-p #:shape-transform #:shape-style
   #:line-shape #:line-shape-p #:line-shape-x1 #:line-shape-y1 #:line-shape-x2 #:line-shape-y2
   #:poly-shape #:poly-shape-p #:poly-shape-points #:poly-shape-closed
   #:rect-shape #:rect-shape-p #:rect-shape-x #:rect-shape-y #:rect-shape-width
   #:rect-shape-height #:rect-shape-rx #:rect-shape-ry
   #:ellipse-shape #:ellipse-shape-p #:ellipse-shape-cx #:ellipse-shape-cy
   #:ellipse-shape-rx #:ellipse-shape-ry
   #:path-shape #:path-shape-p #:path-shape-commands
   #:text-shape #:text-shape-p #:text-shape-x #:text-shape-y #:text-shape-lines
   #:text-shape-size #:text-shape-char-width #:text-shape-angle #:text-shape-h-align
   #:text-shape-v-align #:text-shape-color #:text-shape-font
   #:image-shape #:image-shape-p #:image-shape-bitmap #:image-shape-x1 #:image-shape-y1
   #:image-shape-x2 #:image-shape-y2
   #:text-line-advance

   ;; Rendering to pixels (wordperfect-graphics/raster)
   #:render-scene
   #:render-png
   #:*font-files*
   #:*max-pixels*

   ;; SVG export
   #:write-scene-svg
   #:write-svg
   #:svg-string))
