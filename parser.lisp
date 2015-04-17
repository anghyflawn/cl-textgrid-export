(in-package #:cl-textgrid-export)

(defun str->number (str)
  (with-input-from-string (in str)
    (read in)))


;; This sets up classes and also methods

(defclass generic-interval ()
  ((xmin
    :initarg :xmin
    :accessor xmin)
   (xmax
    :initarg :xmax
    :accessor xmax)))

(defclass interval (generic-interval)
  ((label
    :initarg :label
    :accessor label)))

(defclass tier (generic-interval)
  ((class
    :initarg :tierclass
    :accessor tierclass)
   (name
    :initarg :name
    :accessor name)
   (contents
    :initarg :contents
    :accessor contents)))

(defclass textgrid (generic-interval)
  ((size
    :initarg :size
    :accessor size)
   (contents
    :initarg :contents
    :accessor contents)))

(defmethod initialize-instance :after ((interval generic-interval) &key &allow-other-keys)
    (setf (xmin interval) (str->number (xmin interval)))
    (setf (xmax interval) (str->number (xmax interval))))
  

(defgeneric empty-p (interval)
  (:documentation "Returns T if there is no label"))

(defmethod empty-p ((interval interval))
  (let ((text (label interval)))
    (or (string= "" text)
	(not (null (cl-ppcre:scan "\\s+" text))))))

(defgeneric tier-by-name (textgrid name)
  (:documentation "Return the first tier with the given name"))

(defmethod tier-by-name ((textgrid textgrid) (name string))
  (find-if (lambda (x)
	     (string= (name x) name)) 
	   (contents textgrid)))

(defgeneric point-in (point interval &key)
  (:documentation "Returns T if time point is contained in interval"))

(defmethod point-in (point (interval interval) &key (strict nil))
  (let ((ls (if strict #'< #'<=))
	(gt (if strict #'> #'>=)))
    (and (funcall gt point (xmin interval))
	 (funcall ls point (xmax interval)))))

(defgeneric overlaps (interval-1 interval-2 &key)
  (:documentation "Tests for interval overlap. See
  http://www.rgrjr.com/emacs/overlap.html"))

(defmethod overlaps ((interval-1 interval) (interval-2 interval) &key (strict nil))
  (let ((ls (if strict #'< #'<=)))
    (and (funcall ls (xmin interval-1) (xmax interval-2))
	 (funcall ls (xmin interval-2) (xmax interval-1)))))


(defun extract-value (plist val)
  (getf
   (find-if (lambda (x)
	      (string= (getf x :attribute) val))
	    plist)
   :value))

(defun extract-string-value (plist val)
  (string-trim '(#\") (extract-value plist val)))

(defun make-interval-from-plist (plist)
  (make-instance 'interval
		 :xmin (extract-value plist "xmin")
		 :xmax (extract-value plist "xmax")
		 :label (extract-string-value plist "text")))


;; This is the parser

(defrule digit (digit-char-p character))

(defrule alphanumeric 
    (alphanumericp character))

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))
		  

(defrule attribute-value-pair 
    (and (* #\Space)
	 (* (not whitespace))
	 " = "
	 (* (not whitespace))
	 (? whitespace))
  (:lambda (list)
    (list :attribute
	  (concatenate 'string (cadr list))
	  :value
	  (concatenate 'string (nth 3 list)))))

(defrule interval-init-line
    (and (* #\Space)
	 "intervals ["
	 (? digit)
	 "]:"
	 (? whitespace))
  (:constant nil))

(defrule tier-init-line
    (and (* #\Space)
	 "item ["
	 (? digit)
	 "]:"
	 (? whitespace))
  (:constant nil))

(defrule interval
    (and interval-init-line
	 (* attribute-value-pair))
  (:destructure (i plist)
     (declare (ignore i))
     (make-interval-from-plist plist)))

(defrule tier
    (and tier-init-line
	 (* attribute-value-pair)
	 (* interval))
  (:destructure (i plist contents)
     (declare (ignore i))
     (make-instance 'tier 
		    :tierclass (extract-string-value plist "class")
		    :name (extract-string-value plist "name")
		    :xmin (extract-value plist "xmin")
		    :xmax (extract-value plist "xmax")
		    :contents contents)))

(defrule textgrid
    (and "File type = \"ooTextFile\""
	 (* whitespace)
	 "Object class = \"TextGrid\""
	 (* whitespace)
	 (* attribute-value-pair)
	 "tiers? <exists>"
	 (* whitespace)
	 (* attribute-value-pair)
	 tier-init-line
	 (* tier))
  (:destructure (a b c d x-plist e f size-plist g contents)
     (declare (ignore a b c d e f g))
     (make-instance 'textgrid 
		    :xmin (extract-value x-plist "xmin")
		    :xmax (extract-value x-plist "xmax")
		    :size (extract-value size-plist "size")
		    :contents contents)))







