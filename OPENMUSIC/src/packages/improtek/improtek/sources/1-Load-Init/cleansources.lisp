(in-package :om)

(if (find-library "Improtek") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "Improtek")))))
(if (find-library "alexandria") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "alexandria")))))
(if (find-library "bordeaux-threads") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "bordeaux-threads")))))
(if (find-library "fiveam") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "fiveam")))))