
(defpackage :cl-ffc
  (:use :cl)
  (:export ; utf-8 file util
           :slurp-utf-8-file

	   ; filename / foldername
	   :filename
	   :foldername

	   ; boring files/directories
	   :*boring-path-regexps*
	   :boring-file-p
	   :non-boring-files
	   :boring-folder-p
	   :non-boring-folders

           ; file function caches
           :ffc
	   :create-ffc
	   :ffc-get
	   :ffc-call
	   :ffc-link
	   :ffc-get-linked
	   :ffc-needs-syncing-p
	   :ffc-sync
	   :ffc-uncache-deleted))

(in-package :cl-ffc)

; configurable

(defvar *boring-path-regexps* (list "^\\." "^\\_" "^#" "~$" ".fasl$")
  "if a file's FILENAME or folder's FOLDERNAME matches one of these then it's
considered \"boring\" and ignored by most things.

note that this is a _compile_time_ variable and you must recompile this whole file
for changes to its value to be noticed
")

; utf-8 file util

(defun slurp-utf-8-file (p)
  "return the contents of the file pointed to by the pathname designator `P' as a string assumes the file is encoded in :utf-8"
  (with-open-file (in p :element-type '(unsigned-byte 8)
		        :direction :input)
    (trivial-utf-8:read-utf-8-string in :stop-at-eof t)))

; filename / foldernames

(defun filename (file)
  "
   (filename \"/foo/bar/baz.txt\")

   -> \"baz.txt\"
"
  (if (pathname-type file)
      (format nil "~A.~A" (pathname-name file) (pathname-type file))
      (pathname-name file)))

(defun foldername (folder)
  "
   (foldername \"/foo/bar/baz/\")

   -> \"baz\"
"
  (first (last (pathname-directory folder))))

; boring files / folders

(let ((scanners (mapcar 'cl-ppcre:create-scanner *boring-path-regexps*)))

  (defun boring-file-p (path)
    "see *BORING-PATH-REGEXPTS*"
    (let* ((filename        (filename path))
	   (filename-length (length filename)))
      (some (lambda (pred) (funcall pred filename 0 filename-length))
	    scanners)))

(defun boring-folder-p (path)
  "see *BORING-PATH-REGEXPTS*"
    (let* ((foldername        (foldername path))
	   (foldername-length (length foldername)))
      (some (lambda (pred) (funcall pred foldername 0 foldername-length))
	    scanners)))
)

(defun non-boring-files (folder)
  "returns a list of all the non-boring files in `FOLDER'"
  (remove-if 'boring-file-p
	     (remove-if 'cl-fad:directory-pathname-p
			(cl-fad:list-directory folder))))

(defun non-boring-folders (folder)
  "returns a list of all the non-boring folders in `FOLDER'"
  (remove-if 'boring-file-p
	     (remove-if-not 'cl-fad:directory-pathname-p
			    (cl-fad:list-directory folder))))

; ffc

(defstruct ffc
  (function-ht (make-hash-table :test 'equal))
  (link-ht (make-hash-table :test 'equal))
  file-fn-factory
  folder-fn-factory)

(defmethod print-object ((ffc ffc) s)
  (format s
	  "#<ffc ~A ~A ~A>"
	  (hash-table-count (ffc-function-ht ffc))
	  (if #1=(ffc-file-fn-factory ffc)
	      (format nil "[file: ~A]" #1#)
	      "")
	  (if #2=(ffc-folder-fn-factory ffc)
	      (format nil "[folder: ~A]" #2#)
	      "")))

(defun create-ffc (&key file-fn-factory folder-fn-factory)
  (make-ffc :file-fn-factory file-fn-factory
	    :folder-fn-factory folder-fn-factory))

(defun ffc-get (ffc path)
  (gethash (namestring path) (ffc-function-ht ffc)))

(defun ffc-call (ffc path &rest args)
  (let ((% (ffc-get ffc path)))
    (if %
	(apply (first %) args))))

(defun ffc-link (ffc path paths)
  (if paths
      (setf (gethash (namestring path) (ffc-link-ht ffc))
	    (mapcar (lambda (p) (cons p (rest (ffc-get ffc p))))
		    paths))))

(defun ffc-get-linked (ffc path)
  (gethash (namestring path) (ffc-link-ht ffc)))

(defun ffc-needs-syncing-p (ffc path)
  (let ((% (gethash (namestring path) (ffc-function-ht ffc)))
	(file-write-date (file-write-date path)))
    (if (null %)
	(values file-write-date t)
	(destructuring-bind (fn . known-write-date) %
	  (declare (ignore fn))
	  (if (or (> file-write-date
		     known-write-date)
		  (find-if (lambda (_) (destructuring-bind (p . known-wd) _
				 (or (not known-wd)
				     (< known-wd
					(file-write-date p)))))
			   (ffc-get-linked ffc path)))
	      file-write-date)))))

(defun ffc-sync (ffc path)
  (values ffc
	  (prog1

	      ;; sync
	      (let ((factory (if (cl-fad:directory-pathname-p path)
				 (ffc-folder-fn-factory ffc)
				 (ffc-file-fn-factory ffc))))
		(if factory
		    (multiple-value-bind (needs-syncing 1st-time-seen)
			(ffc-needs-syncing-p ffc path)
		      (if needs-syncing
			  (multiple-value-bind (fn links)
			      (funcall factory path 1st-time-seen)
			    (prog1
				(setf (gethash (namestring path) (ffc-function-ht ffc))
				      (cons (or fn (constantly nil)) 
					    needs-syncing))
			      (mapc (lambda (_) (ffc-sync ffc _)) links)
			      (ffc-link ffc path links)))))))

	    ;; sync decendents
	    (when (cl-fad:directory-pathname-p path)
	      
	      (dolist (f (non-boring-files path))
		(ffc-sync ffc f))
	      
	      (dolist (f (non-boring-folders path))
		(ffc-sync ffc f))))))

(defun ffc-uncache-deleted (ffc &key hook)
  (flet ((doit (ht)
	   (let (delete)
	     (maphash (lambda (p v)
			(declare (ignore v))
			(if (not (if (cl-fad:directory-pathname-p p)
				     (cl-fad:directory-exists-p p)
				     (cl-fad:file-exists-p p)))
			    (push p delete)))
		      ht)
	     (mapc (lambda (_)
		     (if hook
			 (funcall hook _))
		     (remhash _ ht))
		   delete))))
    (doit (ffc-function-ht ffc))
    (doit (ffc-link-ht ffc))
    (values)))
