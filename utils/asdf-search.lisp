;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pnathan 2012. experimental work to build a utility for finding
;;; broken systems in cl-test-grid.

;; base dependency.
(ql:quickload :cl-graph)

;; unit tests
(ql:quickload :fiveam)
(use-package :fiveam)


(defun find-asdf-depends (system)
  "Returns the list of the asdf dependencies of system"
  ;; Rest of.
  (mapcar #'make-keyword
  (cdr
   ;; second componenent
   (second
    (asdf:component-depends-on
     'asdf:load-op
     system)))))

(defun make-keyword (str)
  (intern (string-upcase str) "KEYWORD"))

(defun add-system-to (graph system)
  "Adds ASDF system to graph.

The system should be specified as a keyword

Mutates state"

  (let* ((asdf-system-keyword
	  (make-keyword (asdf:component-name (asdf:find-system system)))))
    (unless (cl-graph:find-vertex graph asdf-system-keyword nil)

      (format t "adding vertex ~a~% "  asdf-system-keyword)

      (cl-graph:add-vertex graph
        asdf-system-keyword
			   ;; We should have found this one
			   ;; already. Error! Error!
			   :if-duplicate-do :error)

      (format t "~a:  ~A~&" system  (find-asdf-depends (asdf:find-system system)))
      (loop for dep in (find-asdf-depends (asdf:find-system system))
	 do
	   (format t "adding edge between ~a and ~a~&"
		   asdf-system-keyword dep)

	   (add-system-to graph dep)
	   (cl-graph:add-edge-between-vertexes graph asdf-system-keyword dep)))))

(defun find-all-asdf-depending-for-load (system )
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container)))
    (add-system-to graph system)
  graph))


;;; experimental/workbook code
(cl-graph:graph->dot
 (find-all-asdf-depending-for-load :cl-yahoo-finance)
 "~/asdf-dump.dot"
 :edge-labeler #'(lambda (e stream)
		   ;; don't output anything
		   (format stream ""))
 :vertex-labeler
 #'(lambda (v stream)
       (format t "~a~&" (cl-graph:element v))
       (format stream "~a" (cl-graph:element v))
       ))

(asdf:find-system (make-keyword (asdf:component-name (asdf:find-system :spatial-trees))))
