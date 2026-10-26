;;;; png.lisp -- PNG output for decoded bitmaps (system wordperfect-graphics/png).

(in-package #:wordperfect-graphics)

(defun bitmap-png (bitmap oriented)
  (multiple-value-bind (octets w h) (bitmap-rgba-octets bitmap :oriented oriented)
    (make-instance 'zpng:png :width w :height h
                             :color-type :truecolor-alpha
                             :image-data octets)))

(defun write-bitmap-png (bitmap destination &key oriented)
  "Write BITMAP as a PNG to DESTINATION, a pathname or a binary output
stream. ORIENTED: see BITMAP-RGBA-OCTETS."
  (let ((png (bitmap-png bitmap oriented)))
    (if (streamp destination)
        (zpng:write-png-stream png destination)
        (zpng:write-png png destination :if-exists :supersede))
    destination))

(defun bitmap-png-octets (bitmap)
  "BITMAP encoded as PNG, as an octet vector, as stored (not oriented)."
  (let ((path (uiop:with-temporary-file (:pathname p :keep t :type "png") p)))
    (unwind-protect
         (progn (write-bitmap-png bitmap path)
                (source-octets path))
      (uiop:delete-file-if-exists path))))
