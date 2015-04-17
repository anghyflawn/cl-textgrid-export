(in-package #:cl-textgrid-export)

(defstruct formant-data point f1 f2)

(defparameter *search-dir* (cl-fad:directory-exists-p #P"data/"))

(defun collect-formant-data (&optional (dir *search-dir*))
  (defparameter *formant-data* (make-hash-table :test 'equal))
  (cl-fad:walk-directory
   dir 
   (lambda (x)
     (when (and (string= (pathname-type x) "txt")
		(string= (pathname-name x) "formant_data"))
       (iterate (for line in-file x using #'read-line)
		(let* ((l (cl-ppcre:split "\\s+" line))
		       (fname (car l)))
		  (setf (gethash fname *formant-data*)
			(cons (make-formant-data :point (str->number (cadr l))
						 :f1 (str->number (caddr l))
						 :f2 (str->number (cadddr l)))
			      (gethash fname *formant-data*)))))))))


(defun file->textgrid (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      ;; This is rather ugly but I'm not writing an actual accurate
      ;; parser at the moment
      (parse 'textgrid (cl-ppcre:regex-replace-all "intervals: " data "")))))

(defun clean-start-end (interval fallback-interval &optional at-left-side)
  (let ((int (or interval fallback-interval)))
    (when int
	(list :min (xmin int) :max (xmax int))
	(if at-left-side
	    (list :min (xmin int) :max (xmin int))
	    (list :min (xmax int) :max (xmax int))))))

(defun get-formants (filename store)
  (gethash (with-output-to-string (str)
	     (format str "~a.wav" (pathname-name filename)))
	   store))

(defun parse-word (filename)
  (let* ((textgrid (file->textgrid (cl-fad:merge-pathnames-as-file *search-dir* filename)))
	 (consonants (remove-if #'empty-p (contents (tier-by-name textgrid "consonants"))))
	 (vowels (remove-if #'empty-p (contents (tier-by-name textgrid "vowels"))))
	 (v1 (car vowels))
	 (v2 (cadr vowels))
	 (h-intervals (remove-if-not (lambda (x)
				       (string= (label x) "h"))
				     consonants))
	 (formants (get-formants filename *formant-data*)))
    (flet ((prev-int (interval)
	     (find-if (lambda (x)
			(and (<= (xmin x) (xmin interval))
			     (point-in (xmax x) interval)))
		      h-intervals))
	   (next-int (interval)
	     (find-if (lambda (x)
			(and (>= (xmax x) (xmax interval))
			     (point-in (xmin x) interval)))
		      h-intervals))
	   (formant (vowel &optional f2)
	     (let* ((fun (if f2 #'formant-data-f2 #'formant-data-f1))
		    (data (find-if (lambda (x)
				       (point-in (formant-data-point x) vowel))
				     formants)))
	       (when data
		 (funcall fun data)))))
      (let* ((c1 (car (set-difference consonants h-intervals)))
	     (c2 (cadr (set-difference consonants h-intervals)))
	     (v1-pre (prev-int v1))
	     (v1-post (next-int v1))
	     (v2-pre (prev-int v2))
	     (v2-post (next-int v2))
	     (v1-pre-start (getf (clean-start-end v1-pre v1 t) :min))
	     (v1-pre-end (getf (clean-start-end v1-pre v1 t) :max))
	     (v1-post-start (getf (clean-start-end v1-post v1) :min))
	     (v1-post-end (getf (clean-start-end v1-post v1) :max))
	     (v2-pre-start (getf (clean-start-end v2-pre v2 t) :min))
	     (v2-pre-end (getf (clean-start-end v2-pre v2 t) :max))
	     (v2-post-start (getf (clean-start-end v2-post v2) :min))
	     (v2-post-end (getf (clean-start-end v2-post v2) :max)))
	(list
	 :word (label (find-if-not #'empty-p (contents (tier-by-name textgrid "word"))))
	 :v1 (when v1 (label v1))
	 :v2 (when c2 (label v2))
	 :v1-pre-start v1-pre-start
	 :v1-pre-end v1-pre-end
	 :v1-post-start v1-post-start
	 :v1-post-end v1-post-end
	 :v2-pre-start v2-pre-start
	 :v2-pre-end v2-pre-end
	 :v2-post-start v2-post-start
	 :v2-post-end v2-post-end
	 :c1 (when c1 (label c1))
	 :c1-start (when c1 (xmin c1))
	 :c1-end (when c1 (xmax c1))
	 :c2 (when c2 (label c2))
	 :c2-start (when c2 (xmin c2))
	 :c2-end (when c2 (xmax c2))
	 :v1-f1 (when v1 (formant v1))
	 :v1-f2 (when v1 (formant v1 t))
	 :v2-f1 (when v2 (formant v2))
	 :v2-f2 (when v2 (formant v2 t))
	 :context (cond
		    (c2 "cluster")
		    ((null v1) nil)
		    ((string= (label v1) "\\sw") "schwa")
		    ((and c1 
			  (member (label c1) '("b" "d" "g" "v" "\\dh" "n" "r" "l") :test #'equal) "long"))
		    (t "short"))
	 :filename (pathname-name filename)
	 :speaker (car (cl-ppcre:split "-" (pathname-name filename))))))))

(defparameter *csv-header*
'("word" "v1" "v2" "v1.pre.start" "v1.pre.end" 
  "v1.post.start" "v1.post.end" "v2.pre.start" "v2.pre.end"
  "v2.post.start" "v2.post.end" "c1" "c1.start" "c1.end" "c2"
  "c2.start" "c2.end" "v1.f1" "v1.f2" "v2.f1" "v2.f2" "context"
  "filename" "speaker"))

(defun unzip-plist (list)
  (iterate (for element in list)
	   (for n from 0)
	   (when (oddp n)
	     (collect element))))

(defun write-csv-row (filename stream)
  (cl-csv:write-csv-row (unzip-plist (parse-word filename)) :stream stream))

(defun parse-data-dir (&optional (dir (cl-fad:merge-pathnames-as-directory *search-dir*)) (target-file "data-full.csv"))
  (collect-formant-data)     
  (with-open-file (str target-file :direction :output :if-exists :supersede)
    (cl-csv:write-csv-row *csv-header* :stream str)
    (cl-fad:walk-directory dir (lambda (x)
				 (when (string= (pathname-type x) "TextGrid")
				   (write-csv-row x str))))))
