(let ((graph (cl-graph:make-graph 'cl-graph:graph-container)))
  (cl-graph:add-vertex graph (asdf:find-system :batteries)
                       :if-duplicate-do :ignore)
  (cl-graph:add-vertex graph (asdf:find-system :defobject)
                       :if-duplicate-do :ignore)

  graph)



(defun add-system-to (graph system)
  (format t "adding vertex ~a~% "  (asdf:find-system system))

  (cl-graph:add-vertex
   graph (asdf:find-system system)
   :if-duplicate-do :ignore)

  (let ((depends
	 (cdr (second
	       (asdf:component-depends-on 'asdf:load-op (asdf:find-system system))))))
    (when depends
      (list depends
	    (loop for comp in depends
	       ;; its possible nconc is ok here.
	       collect (add-system-to graph comp))))))

(defun find-all-asdf-depending-for-load (system )
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container)))
    (add-system-to graph system)
  graph))
