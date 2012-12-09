#|
todo
get saving to work - cl-fad TODO #1
offer to rewrite links 
add delay (or function to generate delay time)
|#

;;;; phidippus.lisp
(in-package #:phidippus)

(defun main ()
  (format t "~%main --->~%") ;; (format t "~{~A~%~}" (crawl "http://www.google.com"))
  (initialize-element-tag-table)
  (setf l1 (make-root "http://twitter.github.com/bootstrap/"))   ;; (setf l1 (make-root "http://www.reddit.com"))
  (setf m2 (make-instance 'webpage-manager :root-link l1 :depth-limit 0))
  (format t "~%main <---~%"))

(defparameter *element-tag* (make-hash-table :test #'equal))
(defparameter *element-tag-alist* '(("a" . "href")
                            ("img" . "src")
                            ("script" . "src")
                            ("link" . "href")))

(defun make-hash-table-from-links (links)
  (let ((tbl (make-hash-table)))
    (loop for i in links do
      (setf (gethash (url i) tbl) i))
    tbl))

(defclass webpage ()
  ((url :accessor url :initarg :url)
   (depth :accessor depth :initarg :depth)
   (page :accessor page :initarg :page)
   (html :accessor html :initarg :html)
   (links-objects :accessor links-objects :initarg :links-objects :initform '())
   (links-list :accessor links-list :initarg :links-list :initform '())))

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

(defclass webpage-manager ()
  ((root-link :accessor root-link :initarg :root-link :initform (error "The 'root' slot (for the url) needs to be set.")) ;the root url to crawl
   (root-only :accessor root-only :initarg :root-only :initform t) ;only root domain?
   (save-to :accessor save-to :initarg :save-to :initform "") ;where to save files to?
   (pages :accessor pages :initarg :pages :initform '()) ;all viewed pages
   (depth-limit :accessor depth-limit :initarg :depth-limit :initform 1) ;root limit
   (links-seen :accessor links-seen :initarg :links-seen :initform '()) ;links seen
   (links-crawled :accessor links-crawled :initarg :links-crawled :initform '()) ;links crawled
   (links-to-crawl :accessor links-to-crawl :initarg :links-to-crawl :initform '()))) ;links left to crawl

(defmethod initialize-instance :after ((mrmanager webpage-manager) &key)
  (setf (links-to-crawl mrmanager) (push (root-link mrmanager) (links-to-crawl mrmanager)))
  (setf (links-crawled mrmanager) (list (root-link mrmanager)))
  (crawl-loop mrmanager))

(defmethod manage-saving ((mrmanager webpage-manager))
  (format t "saving... NOT ~%"))

(defun get-new-links-sat (links-vetted links-new f)
  (format t "get-new-links-sat --- length links-vetted: ~A~%" (length links-vetted))
  (format t "get-new-links-sat --- length links-new: ~A~%" (length links-new))
  (let* ((hashtbl (make-hash-table-from-links links-vetted))
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

(defun initialize-element-tag-table ()
  (loop for i in *element-tag-alist* do
    (setf (gethash (car i) *element-tag*) (cdr i))))

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
  (if (not (equal "https" (subseq (url page) 0 5)))
      (progn
       (format t "crawl --- crawling url: ~A~%" (url page))
       (multiple-value-bind (body code headers response-uri)
           (drakma:http-request (url page)
                                :parameters '(("charset" . "utf-8")))
         (declare (ignore code headers))
         (if (not (typep body '(simple-array (unsigned-byte 8))))
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
                 (add-links-to-page page (nreverse html-links)))
               page)
             (progn
               (setf (html page) body)
               page))))))
