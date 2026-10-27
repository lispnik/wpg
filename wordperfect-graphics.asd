;;;; wordperfect-graphics.asd -- read WordPerfect Graphics (.wpg) files.
;;;;
;;;;   wordperfect-graphics        the reader: header, records, bitmaps, SVG.
;;;;                               No dependencies.
;;;;   wordperfect-graphics/png    PNG output for bitmaps (zpng).
;;;;   wordperfect-graphics/raster render whole drawings to PNG (cl-vectors, zpb-ttf).
;;;;   wordperfect-graphics/tests  FiveAM suite.
;;;;   wordperfect-graphics/cli    the `wpg' command-line program (clingon).

(asdf:defsystem #:wordperfect-graphics
  :description "Reader for WordPerfect Graphics (WPG) files, versions 1 and 2."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "octet-reader")
                             (:file "color")
                             (:file "model")
                             (:file "rle")
                             (:file "bitmap")
                             (:file "charsets")
                             (:file "wptext")
                             (:file "records")
                             (:file "wpg1")
                             (:file "wpg2")
                             (:file "ole")
                             (:file "read")
                             (:file "scene")
                             (:file "svg"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:wordperfect-graphics/tests))))

(asdf:defsystem #:wordperfect-graphics/png
  :description "PNG output for bitmaps decoded from WPG files."
  :depends-on (#:wordperfect-graphics #:zpng)
  :components ((:module "src" :components ((:file "png")))))

(asdf:defsystem #:wordperfect-graphics/raster
  :description "Render WPG drawings to pixels and PNG, without an SVG renderer."
  :depends-on (#:wordperfect-graphics #:wordperfect-graphics/png
               #:cl-vectors #:cl-paths-ttf #:zpb-ttf)
  :components ((:module "src" :components ((:file "raster")))))

(asdf:defsystem #:wordperfect-graphics/tests
  :description "Tests for wordperfect-graphics."
  :depends-on (#:wordperfect-graphics #:wordperfect-graphics/png
               #:wordperfect-graphics/raster #:wordperfect-graphics/fuzz #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "builder")
                             (:file "reader-tests")
                             (:file "rle-tests")
                             (:file "wpg1-tests")
                             (:file "wpg2-tests")
                             (:file "svg-tests")
                             (:file "fixture-tests")
                             (:file "text-tests")
                             (:file "scene-tests")
                             (:file "fuzz-tests")
                             (:static-file "data/imagemagick-input.wpg")
                             (:static-file "data/libreoffice-wpg1.wpg"))))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :wordperfect-graphics/tests :run-tests)
               (error "wordperfect-graphics test suite failed"))))

(asdf:defsystem #:wordperfect-graphics/fuzz
  :description "Mutation fuzzing for wordperfect-graphics (make fuzz)."
  :depends-on (#:wordperfect-graphics #:wordperfect-graphics/png
               #:wordperfect-graphics/raster #:sb-md5)
  :components ((:module "fuzz" :components ((:file "fuzz")))))

(asdf:defsystem #:wordperfect-graphics/cli
  :description "The `wpg' command: inspect WPG files, extract bitmaps, convert to SVG."
  :depends-on (#:wordperfect-graphics #:wordperfect-graphics/png
               #:wordperfect-graphics/raster #:clingon)
  :components ((:module "cli" :components ((:file "main"))))
  :build-operation "program-op"
  :build-pathname "bin/wpg"
  :entry-point "wordperfect-graphics.cli:main")
