;; 1 та 2 завдання
(defun read-csv-to-hash-table (file-path hash-table key)
  (with-open-file (stream file-path :direction :input)

    (read-line stream)

    (cond
     ((eq key :projects)
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof))
        (let* ((fields (uiop:split-string line :separator ","))
               (id (parse-integer (first fields)))
               (name (string-trim '(#\Space #\Tab #\Newline #\Return) (second fields)))
               (model (string-trim '(#\Space #\Tab #\Newline #\Return) (third fields)))
               (nested-hash (make-hash-table)))
          (setf (gethash :id nested-hash) id)
          (setf (gethash :name nested-hash) name)
          (setf (gethash :model nested-hash) model)
          (setf (gethash id hash-table) nested-hash))))
     
     ((eq key :models)
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof))
        (let* ((fields (uiop:split-string line :separator ","))
               (id (parse-integer (first fields)))
               (model (string-trim '(#\Space #\Tab #\Newline #\Return) (second fields)))
               (nested-hash (make-hash-table)))
          (setf (gethash :id nested-hash) id)
          (setf (gethash :model nested-hash) model)
          (setf (gethash id hash-table) nested-hash))))

     (t (format t "Unknown key: ~A~%" key)))))

;; 3 завдання
(defun select (file-path key &rest filters)
  (lambda (&rest filters)
    (let ((result '())
          (hash-table (make-hash-table :test #'equal)))

      (read-csv-to-hash-table file-path hash-table key)

      (if (null filters)
        (maphash (lambda (key value)
            (push value result))
          hash-table)
          
        (let* ((filter-hash (make-hash-table :test #'equal)))
          (loop for (filter-key filter-value) on filters by #'cddr
                do (setf (gethash filter-key filter-hash) filter-value))

          (maphash (lambda (key value)
                    (let ((nested-hash value)
                          (matches t))
                      (maphash (lambda (filter-key filter-value)
                                  (let ((nested-value (gethash filter-key nested-hash)))
                                    (when (not (equal filter-value nested-value))
                                      (setf matches nil))))
                                filter-hash)
                      (when matches
                        (push nested-hash result))))
                   hash-table)))
          (reverse result))))

;; 4 завдання
(defun write-csv-from-hash-tables (file-path hash-tables)
  (with-open-file (stream file-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let* ((keys (loop for k being the hash-keys of (first hash-tables) collect k))
           (header (mapcar #'string keys)))
      (format stream "~{~a~^,~}~%" header)
      (dolist (hash-table hash-tables)
        (let ((values (mapcar (lambda (key) 
                                (let ((value (gethash key hash-table)))
                                  (if value 
                                      (write-to-string value)
                                      "")))
                              keys)))
          (format stream "~{~a~^,~}~%" values))))))

;; 5 завдання
(defun hash-table-to-alist (hash-table)
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist)))

;; 6 завдання
(defun print-hash-tables (hash-tables)
  (let ((fields (let ((keys '()))
                  (maphash (lambda (key value)
                             (push key keys)) (first hash-tables))
                  (reverse keys))))
    (format t "~%")
    (format t "~{~20A~}" (mapcar #'symbol-name fields))
    (format t "~%")
    (dolist (table hash-tables)
      (let ((values (mapcar (lambda (key) (gethash key table)) fields)))
        (format t "~{~20A~}" values)
        (format t "~%")))))



;; Тести
(defun test-read-filter-data ()
    (format t "~%All data from projects.csv:~%")
    (print-hash-tables (funcall (select "projects.csv" :projects)))

    (format t "~%All data from models.csv:~%")
    (print-hash-tables (funcall (select "models.csv" :models)))

    (format t "~%Projects with EchoGPT model:~%")
    (print-hash-tables (funcall (select "projects.csv" :projects) :model "EchoGPT"))

    (format t "~%Projects with SynthAI name:~%")
    (print-hash-tables (funcall (select "projects.csv" :projects) :name "SynthAI"))

    (format t "~%Models with id 1:~%")
    (print-hash-tables (funcall (select "projects.csv" :models) :id 1))
)

(defun test-write-structure-to-csv ()
    (format t "~%Let's output the test data to write to the table:")
    (print-hash-tables (funcall (select "projects.csv" :projects) :model "EchoGPT"))
    
    (write-csv-from-hash-tables "output.csv" (funcall (select "projects.csv" :projects) :model "EchoGPT"))
    
    (with-open-file (stream "output.csv" :direction :input)
      (format t "~%Contents of output.csv~%")
      (loop for line = (read-line stream nil)
            while line
            do (format t "~a~%" line))))

(defun test-hash-table-to-to-alist ()
  (let* ((hash-table (make-hash-table :test 'equal))
         (expected-alist '((:id . 1)
                           (:name . "NeuroVision")
                           (:model . "NeuroFluxNet"))))

    (setf (gethash :id hash-table) 1)
    (setf (gethash :name hash-table) "NeuroVision")
    (setf (gethash :model hash-table) "NeuroFluxNet")

    (let ((generated-alist (hash-table-to-alist hash-table)))
      (if (equal expected-alist generated-alist)
          (format t "The result is correct ~a~%" generated-alist)
          (format t "The result is not correct")))))
