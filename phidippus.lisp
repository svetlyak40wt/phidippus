#|
todo
test other depth levels
get saving to work
offer to rewrite links
add delay (or function to generate delay time)
|#


;;;; phidippus.lisp
(in-package #:phidippus)

(defun main ()
  (format t "~%main --->~%")  ;; (format t "~{~A~%~}" (crawl "http://www.google.com"))  ;; (format t "~{~A~%~}" (crawl "http://www.google.com"))
  (initialize-element-tag-table)
  (setf l1 (make-root "http://twitter.github.com/bootstrap/"))
  (print-manager-links (make-instance 'webpage-manager :root-link l1 :depth-limit 0))
  (format t "~%main <---~%"))

(defun make-root (url)
  (make-instance 'webpage :url url :depth 0))

(defun make-link (&key url depth)
  (make-instance 'webpage :url url :depth depth))

(defclass webpage ()
  ((url :accessor url :initarg :url)
   (depth :accessor depth :initarg :depth)
   (page :accessor page :initarg :page)
   (html :accessor html :initarg :html)
   (links-objects :accessor links-objects :initarg :links-objects :initform '())
   (links-list :accessor links-list :initarg :links-list :initform '())))

(defmethod print-links ((page webpage))
  (format t "~{~A~%~}" (links-list page)))

(defmethod print-manager-links ((mrmanager webpage-manager))
  (loop for i in (links-crawled mrmanager) do
    (print-links i)))

(defmethod initialize-instance :after ((page webpage) &key)
  (loop for i in (links-list page) do
    (setf (links-objects page) (push (make-link :url i :depth (depth (link page))) (links-objects page)))))

(defun correct-root (root)
  root)

(defclass webpage-manager ()
  ((root-link :accessor root-link :initarg :root-link :initform (error "The 'root' slot (for the url) needs to be set.")) ;the root url to crawl
   (root-only :accessor root-only :initarg :root-only :initform t) ;only root domain?
   (save-to :accessor save-to :initarg :save-to :initform "") ;where to save files to?
   (pages :accessor pages :initarg :pages :initform '()) ;all viewed pages
   (depth-limit :accessor depth-limit :initarg :depth-limit :initform 1) ;root limit
   (links-seen :accessor links-seen :initarg :links-seen :initform '()) ;links seen
   (links-crawled :accessor links-crawled :initarg :links-crawled :initform '()) ;links crawled
   (links-to-crawl :accessor links-to-crawl :initarg :links-to-crawl :initform '()))) ;linkes left to crawl

(defmethod initialize-instance :after ((mrmanager webpage-manager) &key)
  (setf (links-to-crawl mrmanager) (push (root-link mrmanager) (links-to-crawl mrmanager)))
  (crawl-loop mrmanager))

(defmethod manage-saving ((mrmanager webpage-manager))
  (format t "saving... NOT ~%"))

(defun make-hash-table-from-links (links)
  (let ((tbl (make-hash-table)))
    (loop for i in links do
      (setf (gethash (url i) tbl) t))
    tbl))

(defun diff-links (links1 links2)
  (let* ((lbig (if (> (length links1) (length links2))
                   links1
                   links2))
         (lsmall (if (< (length links1) (length links2))
                     links1
                     links2))
         (hashtbl (make-hash-table-from-links lbig))
         (newlinks lbig))
    (loop for i in lsmall do
      (if (not (gethash (url i) hashtbl))
          (setf newlinks (push i newlinks))))))

(defmethod addpage ((mrmanager webpage-manager) (page webpage))
  (format t "~%addpage --->~%")
  (progn
    (setf (links-to-crawl mrmanager) (cdr (links-to-crawl mrmanager)))
    (setf (links-seen mrmanager) (diff-links (links-objects page) (links-seen mrmanager)))
    (setf (links-crawled mrmanager) (push page (links-crawled mrmanager))))
  (format t "~%addpage <----~%")
  mrmanager)

(defparameter *element-tag* (make-hash-table :test #'equal))
(defparameter *element-tag-alist* '(("a" . "href")
                            ("img" . "src")
                            ("script" . "src")
                            ("link" . "href")))

(defun initialize-element-tag-table ()
  (loop for i in *element-tag-alist* do
    (setf (gethash (car i) *element-tag*) (cdr i))))

(defmethod crawl-loop ((mrmanager webpage-manager))
  (with-slots (root root-only save-to pages depth-limit links-seen links-crawled links-to-crawl) mrmanager
    (format t "crawl-loop --->~%")
    (cond ((> (length links-to-crawl) 0)
           (addpage mrmanager (crawl (car links-to-crawl))))
          ((eql (length links-to-crawl) 0)
           (manage-saving mrmanager)))
    (format t "crawl-loop <---~%")))

(defun crawl (page)
  (multiple-value-bind (body code headers response-uri)
      (drakma:http-request (url page)
                           :parameters '(("charset" . "utf-8")))
    (declare (ignore code headers))
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
            (push (puri:render-uri (puri:merge-uris link response-uri) nil)
                  html-links))))
      (progn
        (setf (html page) body)
        (setf (links-list page) (nreverse html-links)))
      page)))
