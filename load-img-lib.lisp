
(in-package :cl-user)
(defun load-img-lib ()
  (push "~/lisp-project/ch-image_0.4.1/" asdf:*central-registry*)
  (push "~/lisp-project/ch-util_0.3.10/" asdf:*central-registry*)
  (push "~/lisp-project/clem_0.4.1/" asdf:*central-registry*)
  (push "~/lisp-project/cl-jpeg/" asdf:*central-registry*)
  (push "~/lisp-project/salza2-2.0.8/" asdf:*central-registry*)
  (push "~/lisp-project/zpng-1.2.1/" asdf:*central-registry*)
  (asdf:load-system 'ch-image))

(defun read-img ()
  (ch-image:ARGB-IMAGE-TO-GRAY-IMAGE (ch-image:read-image-file "/home/xx/lisp-project/mm.jpg")))

(defun read-cubic-img ()
  (ch-image:ARGB-IMAGE-TO-GRAY-IMAGE (ch-image:read-image-file "~/lisp-project/IMG_20120826_212625.jpg")))
