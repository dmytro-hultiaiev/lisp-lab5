<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Гультяєв Дмитро</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
## Варіант 5
<b>База даних:</b> Проєкти із застосуванням ШІ 

<b>Тип записів:</b> Геш-таблиця

<b>Таблиці:</b> Проєкти, Моделі штучного
## Лістинг реалізації завдання
```lisp
;; 1 та 2 завдання
(defvar *table1* (make-hash-table :test #'equal))
(defvar *table2* (make-hash-table :test #'equal))

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
          (hash-table (case key
                        (:projects *table1*)
                        (:models *table2*)
                        (otherwise (error "Unknown key ~A" key)))))

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
                                    (when (and nested-value
                                              (not (string= (write-to-string filter-value)
                                                            (write-to-string nested-value))))
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

```
### Тестові набори та утиліти
```lisp
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
```
### CSV таблиця projects.csv
```csv
id,name,model
1,NeuroVision,NeuroFluxNet
2,SynthAI,NeuroFluxNet
3,EchoBrain,EchoGPT
4,QuantumNet,EchoGPT
```
### CSV таблиця models.csv
```csv
id,model
1,NeuroFluxNet
2,EchoGPT
```

### Тестування
```lisp
CL-USER> (test-read-filter-data)

All data from projects.csv:

ID                  NAME                MODEL               
1                   NeuroVision         NeuroFluxNet        
2                   SynthAI             NeuroFluxNet        
3                   EchoBrain           EchoGPT             
4                   QuantumNet          EchoGPT             

All data from models.csv:

ID                  MODEL               
1                   NeuroFluxNet        
2                   EchoGPT             

Projects with EchoGPT model:

ID                  NAME                MODEL               
3                   EchoBrain           EchoGPT             
4                   QuantumNet          EchoGPT             

Projects with SynthAI name:

ID                  NAME                MODEL               
2                   SynthAI             NeuroFluxNet

Models with id 1:

ID                  MODEL               
1                   NeuroVision

CL-USER> (test-write-structure-to-csv)

Let's output the test data to write to the table:
ID                  NAME                MODEL               
3                   EchoBrain           EchoGPT             
4                   QuantumNet          EchoGPT             

Contents of output.csv
ID,NAME,MODEL
3,"EchoBrain","EchoGPT"
4,"QuantumNet","EchoGPT"

CL-USER> (test-hash-table-to-to-alist)
The result is correct ((ID . 1) (NAME . NeuroVision) (MODEL . NeuroFluxNet))
```  
