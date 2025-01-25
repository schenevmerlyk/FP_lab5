(defstruct manufacturer
  id
  name
  country
  established-year)

(defstruct drone
  id
  name
  type
  manufacturer-id
  description)

(defun hash-table-keys (hash-table)
  (let (keys)
    (maphash (lambda (key _)
               (push key keys))
             hash-table)
    (reverse keys)))

(defun mapconcat (function list separator)
  (apply #'concatenate
         'string
         (loop for element in list
               append (list (funcall function element) separator)
               into result
               finally (return (butlast result)))))



(defun read-csv-as-hash-table (file-path &key (delimiter #\,))
  (with-open-file (stream file-path :direction :input)
    (let* ((lines (loop for line = (read-line stream nil nil)
                        while line
                        collect line))
           (headers (split-string (first lines) delimiter)))
      (mapcar (lambda (line)
                (let ((values (split-string line delimiter)))
                  (let ((record (make-hash-table :test 'equal)))
                    (loop for header in headers
                          for value in values
                          do (setf (gethash header record) value))
                    record)))
              (rest lines))))) ; Обробляємо всі рядки, крім першого

(defun write-records-to-csv (file-path records &key (delimiter #\,) (columns nil))
  (let ((columns (or columns (and records (hash-table-keys (first records))))))
    (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      ;; Записуємо заголовки
      (write-line (mapconcat #'identity columns (string delimiter)) stream)
      ;; Записуємо записи
      (dolist (record records)
        (write-line (mapconcat (lambda (col) (or (gethash col record "") ""))
                               columns
                               (string delimiter))
                    stream)))))

(defun select (file-path filter-criteria)
  (let ((table (read-csv-as-hash-table file-path)))
    (lambda (&rest filters)
      (let ((filtered-records
              (cond
                ;; Якщо критерій — функція, застосувати її до таблиці
                ((functionp filter-criteria)
                 (remove-if-not filter-criteria table))
                ;; Якщо критерій — асоціативний список
                ((listp filter-criteria)
                 (remove-if-not (lambda (record)
                                  (every (lambda (criterion)
                                           (let ((key (car criterion))
                                                 (value (cdr criterion)))
                                             (equal (gethash key record) value)))
                                         filter-criteria))
                                table))
                ;; Якщо критерій — ключ
                ((stringp filter-criteria)
                 (remove-if-not (lambda (record)
                                  (gethash filter-criteria record))
                                table))
                ;; Інакше повертаємо всю таблицю
                (t table))))
        ;; Додаткове фільтрування за ключовими параметрами
        (loop for record in filtered-records
              when (every (lambda (filter)
                            (let ((key (car filter))
                                  (value (cdr filter)))
                              (equal (gethash key record) value)))
                          filters)
              collect record)))))

(defun records-to-alists (records)
  (mapcar (lambda (record)
            (let ((alist '()))
              (maphash (lambda (key value)
                         (push (cons key value) alist))
                       record)
              (reverse alist)))
          records))

(defun pretty-print-table (records)
  (when records
    (let* ((columns (hash-table-keys (first records))) ; Отримуємо ключі з першого запису
           (column-widths (mapcar
                           (lambda (col)
                             (max (length col)
                                  (reduce #'max records
                                          :key (lambda (record)
                                                 (length (or (gethash col record "") ""))))))
                           columns))
           (row-format (concatenate 'string
                                    (mapcar (lambda (width)
                                              (format nil "~vA" (+ width 2)))
                                            column-widths))))
      (format t "~&")
      (apply #'format t (concatenate 'string row-format "~%")
             (mapcar #'string-upcase columns))
      (format t "~&~{~a~}" (mapcar (lambda (width) (make-string (+ width 2) :initial-element #\-))
                                   column-widths))
      (dolist (record records)
        (apply #'format t (concatenate 'string row-format "~%")
               (mapcar (lambda (col) (or (gethash col record "") "")) columns))))))

(defvar *manufacturers* (read-csv-file "manufacturers.csv" #'create-manufacturer-record))
(defvar *drones* (read-csv-file "drones.csv" #'create-drone-record))

;;; Вивід даних
(format t "Manufacturers and Drones:~%")
(pretty-print-table *manufacturers*)
(pretty-print-table *drones*)

(let ((select-drones (select "drones.csv" #'create-drone-record)))
  (let ((filtered-drones (funcall select-drones '(type . "Quadcopter"))))
    (if filtered-drones
        (progn
          (format t "Filtered Drones:~%")
          (pretty-print-records filtered-drones)
          (write-records-to-csv "filtered_drones.csv" filtered-drones))
        (format t "No drones found for type = Quadcopter~%"))))