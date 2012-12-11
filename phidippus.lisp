#|
todo
be able to handle broken urls/improper data
make sure saving in the dir tree works for multiple domain links
offer to rewrite links relative to each other when saved
add delay (or function to generate delay time)
|#

;;;; phidippus.lisp
(in-package #:phidippus)

(log4cl:config :properties "logs/log4cl.properties" :watch)


;; Utilities
(defmacro debug-print (string)
  `(when (log4cl:debug)
    (log4cl:debug ,string)))

(defun correct-filename (filename)
  (if (eql (subseq filename (1- (length filename))) #\/)
      (subseq 0 (1- (length filename)))
      filename))

;; For writing a string to a file
;; found @http://www.socher.org/index.php/Main/WriteToFileInLisp
(defun write-to-file (filename content)
  (format t "write-to-file --- got here, filename: ~A~%" filename)
  (format t "write-to-file --- type-of content: ~A~%" (reverse (rest (reverse (type-of content)))))
  (if (not (typep content '(simple-array (unsigned-byte 8))))
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create)
        (format stream content))
      (with-open-file (stream filename
                              :direction :output
                              :element-type '(unsigned-byte 8))
        (write-sequence content stream)))
  filename)

;;
;; An easy way of calling the library for testing
;;
(defun test ()
  (debug-print "~%main --->~%")
  (initialize-element-tag-table)
  ;;(setf l1 (make-root "http://twitter.github.com/bootstrap/"))   ;; (setf l1 (make-root "http://www.reddit.com"))
  ;; (setf l1 (make-root "http://www.gigamonkeys.com/"))
  (setf l1 (make-root "http://www.google.com"))
  ;; (setf l1 (make-root "http://gmaps-samples.googlecode.com/svn/trunk/slides/images/google-logo.png"))
  (setf m2 (make-instance 'webpage-manager :root-link l1 :depth-limit 1 :save-to "/tmp/test" :overwrite t :save-flat nil))
  (format t "~%main <---~%"))


;; ---------------------------------------------------------------------------->
;; webpage -------------------------------------------------------------------->
;; ---------------------------------------------------------------------------->
;;
;; The webpage objects for storing url, html, outgoing lengths and depth
;;
(defclass webpage ()
  ((url :accessor url :initarg :url)
   (depth :accessor depth :initarg :depth)
   (page :accessor page :initarg :page)
   (html :accessor html :initarg :html)
   (links-objects :accessor links-objects :initarg :links-objects :initform '())
   (links-list :accessor links-list :initarg :links-list :initform '())))

(defun make-hash-table-from-links (links)
  (let ((tbl (make-hash-table)))
    (loop for i in links do
         (setf (gethash (url i) tbl) i))
    tbl))

;; methods for webpage objects ------------------------------------------------>
(defmethod add-links-to-page ((page webpage) lst)
  (setf (links-list page) lst)
  (loop for i in lst do
       (setf (links-objects page) (push (make-link :url i :depth (+ 1 (depth page))) (links-objects page)))))

;; generating links
(defun make-root (url)
  (make-instance 'webpage :url url :depth 0))
(defun make-link (&key url depth)
  (make-instance 'webpage :url url :depth depth))

;; printing links
(defmethod print-link ((page webpage))
  ;; (format t "~{~A~%~}" (url page))
  (format t "~A~%" (url page)))

(defun print-links (lst)
  (loop for i in lst do
       (print-link i)))

;; <----------------------------------------------------------------------------
;; webpage <--------------------------------------------------------------------
;; <----------------------------------------------------------------------------

;; ---------------------------------------------------------------------------->
;; webpage manager ------------------------------------------------------------>
;; ---------------------------------------------------------------------------->
;;
;; The webpage manager stores all of the webpages and the current
;; state of the crawl
;;
(defclass webpage-manager ()
  ((root-link :accessor root-link :initarg :root-link :initform (error "The 'root' slot (for the url) needs to be set.")) ;the root url to crawl
   (root-only :accessor root-only :initarg :root-only :initform t) ;only root domain?
   (save-to :accessor save-to :initarg :save-to :initform "") ;where to save files to?
   (pages :accessor pages :initarg :pages :initform '()) ;all viewed pages
   (depth-limit :accessor depth-limit :initarg :depth-limit :initform 1) ;root limit
   (links-seen :accessor links-seen :initarg :links-seen :initform '()) ;links seen
   (links-crawled :accessor links-crawled :initarg :links-crawled :initform '()) ;links crawled
   (overwrite :accessor overwrite :initarg :overwrite :initform nil) ;delete and create new dir
   (save-flat :accessor save-flat :initarg :save-flat :initform nil) ;delete and create new dir
   (links-to-crawl :accessor links-to-crawl :initarg :links-to-crawl :initform '()))) ;links left to crawl

;; !!! A webpage-manager begins execution after creation !!!
(defmethod initialize-instance :after ((mrmanager webpage-manager) &key)
  (setf (links-to-crawl mrmanager) (push (root-link mrmanager) (links-to-crawl mrmanager)))
  (crawl-loop mrmanager))




;; for saving pages  ---------------------------------------------------------->
(defmethod manage-saving ((mrmanager webpage-manager))
  (format t "manage-saving --- dir to save to: ~A~%" (save-to mrmanager))
  (format t "manage-saving --- overwrite: ~A~%" (overwrite mrmanager))
  (let ((dir-handle nil))
    (cond ((not (cl-fad:directory-exists-p (save-to mrmanager)))
           ;;if the directory doesn't exist and saving is okay
           (setf dir-handle (ensure-directories-exist (cl-fad:pathname-as-directory (save-to mrmanager)))))
          ((and (not (overwrite mrmanager)) (cl-fad:directory-exists-p (save-to mrmanager)))
           ;;if we can't overwrite and the directory exists
           (format t "The folder '~A' already exists, please specify overwrite status or a new directory name."))
          ((and (overwrite mrmanager) (cl-fad:directory-exists-p (save-to mrmanager)))
           ;;if overwriting is okay and the directory exists
           (cl-fad:delete-directory-and-files (save-to mrmanager))
           (setf dir-handle (ensure-directories-exist (cl-fad:pathname-as-directory (save-to mrmanager))))))
    (if dir-handle
        ;; save to the directory
        (populate-directory mrmanager dir-handle))))

(defmethod populate-directory ((mrmanager webpage-manager) (dir-handle pathname))
  (let ((hashtbl (make-hash-table-from-links (links-crawled mrmanager))))
    (if (save-flat mrmanager)
        ;; save it flat (all within the specified dir-handle)
        (populate-directory-flat mrmanager dir-handle)
        ;; save it by recreating the uri hierarchy with directories
        (populate-directory-tree mrmanager dir-handle))))

(defmethod populate-directory-flat ((mrmanager webpage-manager) (dir-handle pathname))
  (format t "populate-directory-flat --->~%")
  (loop for page in (links-crawled mrmanager) do
       (format t "populate-directory-flat --- writing page with url: ~A~%" (url page))
       (let ((filename (merge-pathnames dir-handle (cl-ppcre:regex-replace-all "\\W" (subseq (url page) 7) "-"))))
         (format t "populate-directory-flat --- writing page with filename: ~A~%" filename)
         (write-to-file filename (html page)))))

(defun save-page-to-path (page dir-handle path)
  (format t "save-page-to-path --->~%")
  (labels ((save-rest (page pathcur pathleft)
             (format t "save-page-to-path --- 2~%")
             (format t "save-page-to-path --- type-of pathcur: ~A~%" pathcur)
             (format t "save-page-to-path --- type-of (car pathleft): ~A~%" (type-of (car pathleft)))
             (let ((newpath (merge-pathnames pathcur (car pathleft))))
               (format t "save-page-to-path --- 3~%")
               (format t "save-page-to-path --- type-of newpath: ~A~%" (type-of newpath))
               (format t "save-page-to-path --- newpath: ~A~%"  newpath)
               (format t "save-page-to-path --- type-of (cdr pathleft): ~A~%" (type-of (cdr pathleft)))
               (format t "save-page-to-path --- (cdr pathleft): ~A~%" (cdr pathleft))
               (if (null (cdr pathleft))
                   (progn
                     (format t "save-page-to-path --- 3.5~%")
                     (write-to-file (namestring newpath) (html page))) ;; if this is the last element save the filename there
                   (if (cl-fad:directory-exists-p newpath)
                       (progn
                         (format t "save-page-to-path --- 4~%")
                         (save-rest page newpath (cdr pathleft)))
                       (progn
                         (format t "save-page-to-path --- 5~%")                         
                         (save-rest page (ensure-directories-exist (cl-fad:pathname-as-directory newpath)) (cdr pathleft))))))))
    (format t "save-page-to-path --- 6~%")
    (save-rest page dir-handle path)))

(defmethod populate-directory-tree ((mrmanager webpage-manager) (dir-handle pathname))
  (loop for i in (links-crawled mrmanager) do
       (let* ((urn (puri:parse-uri (url i)))
              (path (append (list (puri:uri-host urn)) (cdr (puri:uri-parsed-path urn)))))
         (save-page-to-path i dir-handle path))))

;; for saving pages  <----------------------------------------------------------



;; for adding pages to webpage-manager ---------------------------------------->
(defmethod addpage ((mrmanager webpage-manager) (page webpage))
  (format t "~%addpage --->~%")
  (setf (links-seen mrmanager) (get-new-links-sat (links-seen mrmanager) (links-objects page) (lambda (x) t)))
  (setf (links-crawled mrmanager) (push page (links-crawled mrmanager)))
  (format t "addpage --- (length (links-list page)): ~A~%" (length (links-list page)))
  (format t "addpage --- (length (links-objects page)): ~A~%" (length (links-objects page)))
  (format t "addpage --- (length (links-seen mrmanager)): ~A~%" (length (links-seen mrmanager)))
  (format t "addpage --- (length (links-to-crawl mrmanager)): ~A~%" (length (links-to-crawl mrmanager)))
  (setf (links-to-crawl mrmanager) (get-new-links-sat
                                    (cdr (links-to-crawl mrmanager))
                                    (links-objects page)
                                    (lambda (x) (if (<= (depth x) (depth-limit mrmanager)) t))))
  (format t "~%addpage <----~%")
  mrmanager)

(defmethod addpage ((mrmanager webpage-manager) (page null))
  (format t "addpage - null hit~A~%" (null page))
  (setf (links-to-crawl mrmanager) (cdr (links-to-crawl mrmanager)))
  mrmanager)

(defun get-new-links-sat (links-vetted links-new f)
  (let ((hashtbl (make-hash-table-from-links links-vetted))
        (to-return '()))
    (loop for i in links-new do
         (cond
           ((and
             (funcall f i)
             (not (gethash (url i) hashtbl)))
            (setf (gethash (url i) hashtbl) i))
           ((and
             (gethash (url i) hashtbl)
             (< (depth (gethash (url i) hashtbl)) (depth i)))
            (setf (gethash (url i) hashtbl) i))))
    (maphash #'(lambda (k v) (setf to-return (push v to-return))) hashtbl)
    ;; (format t "length of to-return: ~A~%" (length to-return))
    to-return))
;; for adding pages to webpage-manager <----------------------------------------



;; hashtable for element lookups in crawl ------------------------------------->
(defparameter *element-tag* (make-hash-table :test #'equal))
(defparameter *element-tag-alist* '(("a" . "href")
                                    ("img" . "src")
                                    ("script" . "src")
                                    ("link" . "href")))

(defun initialize-element-tag-table ()
  (loop for i in *element-tag-alist* do
       (setf (gethash (car i) *element-tag*) (cdr i))))
;; hashtable for element lookups in crawl <-------------------------------------



;; for crawling --------------------------------------------------------------->
(defmethod crawl-loop ((mrmanager webpage-manager))
  (format t "crawl-loop --->~%")
  (format t "crawl-loop --- links-to-crawl: ~A~%" (length (links-to-crawl mrmanager)))
  (format t "crawl-loop --- links-crawled: ~A~%" (length (links-crawled mrmanager)))
  (format t "crawl-loop --- links-seen length ~A~%" (length (links-seen mrmanager)))
  (cond ((> (length (links-to-crawl mrmanager)) 0)
         (crawl-loop (addpage mrmanager (crawl (car (links-to-crawl mrmanager))))))
        ((eql (length (links-to-crawl mrmanager)) 0)
         (manage-saving mrmanager)))
  (format t "crawl-loop <---~%"))

(defun crawl (page)
  (if (and (not (equal "https" (subseq (url page) 0 5)))
           (not (equal "mailto" (subseq (url page) 0 6))))
      (progn
        (format t "crawl --- crawling url: ~A~%" (url page))
        (multiple-value-bind (body code headers response-uri)
            (drakma:http-request (url page)
                                :parameters '(("charset" . "utf-8")))
          (declare (ignore code headers))
          (if (and (not (typep body '(simple-array (unsigned-byte 8))))
                   (not (typep body '(and (vector (unsigned-byte 8)) (not simple-array)))))
              (let ((document (closure-html:parse body (cxml-stp:make-builder)))
                    html-links)
               (cxml-stp:do-recursively (a document)
                 (let* ((element
                         (if (not (typep a 'cxml-stp:element))
                             nil
                             (cxml-stp:local-name a)))
                        (tag
                         (gethash element *element-tag*))
                        (link
                         (if tag
                              (cxml-stp:attribute-value a tag))))
                   (when link
                     (if (not (equal "javascript:void(0);" link))
                         (push (puri:render-uri (puri:merge-uris link response-uri) nil)
                               html-links)))))
               (progn
                 (setf (html page) body)
                 (add-links-to-page page (nreverse html-links)))
               page)
              (progn
                (setf (html page) body)
                page))))))

;; for crawling <---------------------------------------------------------------

;; <----------------------------------------------------------------------------
;; methods for webpage manager <------------------------------------------------
;; <----------------------------------------------------------------------------
