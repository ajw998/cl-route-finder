(defvar *map-data* nil)

(defun add-station (from to time)
  (if (not (member (list from to time) *map-data* :test 'equal))
     (progn 
        (push (list from to time) *map-data*) 
        (push (list to from time) *map-data*))
     nil))

(defun add-item (item queue)
  (if (null queue)
      (cons item queue)
    (if (< (first item) (first (first queue)))
        (cons item queue)
        (cons (first queue) (add-item item (rest queue))))))

(defun add-to-queue (time location via queue)
  (setq queue (add-item (list time location via) queue))
  queue)

(defun add-stations-to-queue (location start queue)
  (dolist (item *map-data* queue)
    (let* ((from (first item))
           (to (second item))
           (time (third item))
           )
      (when (eq from location) (setq queue (add-to-queue (+ start time) to location queue))))))

(defun grow (from to)
  (let* ((visited (list (cons from nil)))
         (queue (add-stations-to-queue from 0 nil))
         w)
    (loop
     (when (eq from to) (return (reverse visited)))
     (unless queue (return))
     (setq w (first queue))
     (setq from (second w))
     (setq queue (cdr queue))
     (unless (assoc from visited)
       (setq visited (cons (cons from (third w)) visited))
       (setq queue (add-stations-to-queue from (car w) queue))))))

(defun get-route (from to)
  (let* ((visited (grow from to))
         route)
    (when visited 
      (loop 
        (push to route)
        (when (eq from to) (return route))
        (setq to (cdr (assoc to visited)))))))

;; REPL
(setq *map-data* nil)

;; Walking distance
(add-station 'tottenham-court-road 'holborn 10)
(add-station 'tottenham-court-road 'oxford-circus 9)
(add-station 'oxford-circus 'bond-street 7)
(add-station 'oxford-circus 'piccadilly-circus 12)
(add-station 'piccadilly-circus 'leicester-square 6)
(add-station 'leicester-square 'tottenham-court-road 8)
(add-station 'bond-street 'marbel-arch 7)
(add-station 'marbel-arch 'lancaster-gate 15)
(add-station 'covent-garden 'holborn 8)
(add-station 'covent-garden 'leicester-square 4)

(get-route 'oxford-circus 'leicester-square) ;; (OXFORD-CIRCUS TOTTENHAM-COURT-ROAD LEICESTER-SQUARE)
(get-route 'covent-garden 'tottenham-court-road) ;; (COVENT-GARDEN LEICESTER-SQUARE TOTTENHAM-COURT-ROAD)

