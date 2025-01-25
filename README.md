## МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС

### Звіт з лабораторної роботи 5
 "Робота з базою даних"
 дисципліни "Вступ до функціонального програмування"

**Студент**: *Петраш Павло Степанович КВ-13*


**Рік**: *2025*

## Завдання:
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

Варіант 14(2): 
База даних: Виробництво дронів
Тип записів: Геш-таблиця
Таблиці: ВИробники дронів, Дрони
Опис: База даних виробників дронів та, власне, дронів.

**Код завдання:**
```
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

```
**Тестові файли:**
"Drones.CSV:"
```
ID,Name,Type,Manufacturer ID,Description
1,Mavic Air 2,Quadcopter,1,Compact and powerful drone for photography.
2,ANAFI,Quadcopter,2,Foldable drone with 4K HDR video.
3,Puma 3 AE,Fixed-Wing,3,Military-grade drone for surveillance.
4,EVO Lite,Quadcopter,4,Advanced drone with superior flight range.
5,X2D,Quadcopter,5,AI-driven drone for autonomous missions.
```
"Manufacturers.CSV:"
```
ID,Name,Country,Established Year
1,DJI,China,2006
2,Parrot,France,1994
3,AeroVironment,USA,1971
4,Autel Robotics,USA,2014
5,Skydio,USA,2014
```
**виведення диних:**
```
Manufacturers and Drones:
Record of type MANUFACTURER:
  ID: 1
  NAME: DJI
  COUNTRY: China
  ESTABLISHED-YEAR: 2006

Record of type MANUFACTURER:
  ID: 2
  NAME: Parrot
  COUNTRY: France
  ESTABLISHED-YEAR: 1994

Record of type MANUFACTURER:
  ID: 3
  NAME: AeroVironment
  COUNTRY: USA
  ESTABLISHED-YEAR: 1971
```

