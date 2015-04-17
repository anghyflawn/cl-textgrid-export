;;;; cl-textgrid-export.asd

(asdf:defsystem #:cl-textgrid-export
  :serial t
  :description "Read a collection of TextGrid and formant_data.txt
  files and put them in a .CSV file"
  :author "Pavel Iosad <pavel.iosad@ed.ac.uk>"
  :license "CRAPL"
  :depends-on (#:cl-ppcre
               #:esrap
	       #:cl-fad
	       #:iterate
	       #:cl-csv)
  :components ((:file "package")
               (:file "parser")
	       (:file "reader")))
