;;;; model.lisp -- the objects a parse produces.

(in-package #:wordperfect-graphics)

(defstruct wpg-header
  "The 16-byte header every WPG file starts with."
  (data-offset 16 :type (unsigned-byte 32))   ; where the first record starts
  (product-type 1 :type (unsigned-byte 8))    ; 1 = WordPerfect
  (file-type 22 :type (unsigned-byte 8))      ; 22 (#x16) = WPG
  (major-version 1 :type (unsigned-byte 8))   ; 1 = WPG1, 2 = WPG2
  (minor-version 0 :type (unsigned-byte 8))
  (encryption-key 0 :type (unsigned-byte 16)) ; nonzero = encrypted
  (reserved 0 :type (unsigned-byte 16)))

(defstruct wpg-record
  "One record from the file.
OFFSET is where the record starts in the file, BODY-OFFSET where its data
starts. TYPE is the numeric record type and NAME its keyword name (or NIL
for a type this library does not know). CLASS and EXTENSION are only
present in WPG2 files; a WPG2 record with a nonzero EXTENSION is followed
by that many child records, and each child's PARENT is the index of the
record it belongs to (NIL at the top level), DEPTH its nesting level. BODY holds the record's raw data bytes. FIELDS is a
plist of decoded values (see RECORD-FIELD), and ERROR is the condition if
decoding the body failed -- the record is still listed."
  (index 0)
  (offset 0)
  (body-offset 0)
  (class nil)
  (type 0)
  (extension nil)
  (parent nil)
  (depth 0)
  (length 0)
  (body (make-octets 0) :type octets)
  (name nil)
  (fields nil)
  (error nil))

(defun record-field (record key &optional default)
  "The decoded field KEY of RECORD."
  (getf (wpg-record-fields record) key default))

(defstruct wpg-bitmap
  "A decoded raster image.
DATA holds HEIGHT rows of ROW-BYTES bytes each, top row first. DEPTH is
bits per pixel and FORMAT says what the bits mean:
  :INDEXED  palette indices (depth 1, 2, 4 or 8), packed high bit first
  :RGB      R, G, B bytes (24)
  :RGBA     R, G, B, opacity bytes (32; WPG2 only)
  :GRAY     one gray byte (8; WPG2 only)
  :GRAY16   16-bit little-endian gray (16; WPG2 only)
  :RGB555   16-bit little-endian, 5 bits each of R, G, B (16; WPG2 only)
PALETTE is the 256-entry palette in effect when the bitmap was read. X1/Y1/X2/Y2 give the bitmap's
placement in file units when the file says, else NIL. COMPLETE-P is false
when the compressed data ran out before the last row."
  (width 0)
  (height 0)
  (depth 1)
  (format :indexed)
  (h-resolution 75)
  (v-resolution 75)
  (palette nil)
  (data (make-octets 0) :type octets)
  (row-bytes 0)
  (x1 nil) (y1 nil) (x2 nil) (y2 nil)
  (rotation 0)
  (mirror-x nil)
  (mirror-y nil)
  (compression :rle)
  (complete-p t)
  (record-index nil))

(defstruct wpg-image
  "A parsed WPG file.
VERSION is 1 or 2. WIDTH and HEIGHT are the drawing's size in file units;
X-UNITS and Y-UNITS are file units per inch (always 1200 in WPG1). The
X-OFFSET and Y-OFFSET are the drawing's lower-left corner (WPG2 only; 0 in
WPG1). PALETTE is the palette as it stands at the end of the file.
WARNINGS lists the problems noticed while reading, as strings."
  header
  (version 1)
  (records #() :type vector)
  (bitmaps nil)
  (width 0)
  (height 0)
  (x-units 1200)
  (y-units 1200)
  (x-offset 0)
  (y-offset 0)
  (double-precision nil)
  palette
  (container nil)
  (warnings nil))

(defun wpg-image-width-inches (image)
  (/ (wpg-image-width image) (max 1 (wpg-image-x-units image))))

(defun wpg-image-height-inches (image)
  (/ (wpg-image-height image) (max 1 (wpg-image-y-units image))))

(defun record-children (image record)
  "The records whose parent is RECORD, in file order."
  (let ((index (wpg-record-index record)))
    (remove-if-not (lambda (r) (eql (wpg-record-parent r) index))
                   (coerce (wpg-image-records image) 'list))))

(defun find-records (image name-or-type)
  "The records of IMAGE whose name (a keyword) or numeric type matches."
  (remove-if-not (lambda (r)
                   (if (integerp name-or-type)
                       (eql (wpg-record-type r) name-or-type)
                       (eq (wpg-record-name r) name-or-type)))
                 (coerce (wpg-image-records image) 'list)))
