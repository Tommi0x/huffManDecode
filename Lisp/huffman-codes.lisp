;;;; Toloni_Tommaso_914293
;;;; Boschi_Leonardo_914255

;;;; huffman-codes.lisp 

;; struttura nodo albero huffman
(defstruct node
  (symbol nil)     ; Symbol
  (weight 0)       ; Weight 
  (left nil)       ; Left child
  (right nil)      ; Right child
  (leaf-p nil))    ; Flag to identify leaf nodes

;; Construttore foglie
(defun make-leaf (symbol weight)
  (make-node :symbol symbol :weight weight :leaf-p t))

;; Controlla se un nodo e' una foglia
(defun leaf-p (node)
  (node-leaf-p node))

;; Ritorna il simbolo di una foglia
(defun leaf-symbol (node)
  (node-symbol node))

;; Ritorna tutti i simboli del sottoalbero
(defun node-symbols (node)
  (if (leaf-p node)
      (list (leaf-symbol node))
      (append (node-symbols (node-left node))
              (node-symbols (node-right node)))))

;; attraversa l'albero in base al bit
(defun choose-branch (bit branch)
  (cond ((= 0 bit) (node-left branch))
        ((= 1 bit) (if (node-right branch)
                       (node-right branch)
                       (error "Bit 1 non valido: albero non ha rami destri.")))
        (t (error "bit invalido: ~D." bit))))

;;; FUNZIONI PRINCIPALI

;; hucodec-decode
(defun hucodec-decode (bits huffman-tree)
  (labels ((decode-1 (bits current-branch)
             (if (null bits)
                 nil
                 (let ((next-branch (choose-branch (first bits)
                                              current-branch)))
                   (if (leaf-p next-branch)
                       (cons (leaf-symbol next-branch)
                             (decode-1 (rest bits) huffman-tree))
                       (decode-1 (rest bits) next-branch))))))
    (cond 
      ;; Caso no bit
      ((null bits) nil)
      ;; Caso singolo
      ((and (leaf-p (node-left huffman-tree)) (null (node-right huffman-tree)))
       (make-list (length bits) 
       :initial-element (leaf-symbol (node-left huffman-tree))))
      ;; Caso normale
      (t (decode-1 bits huffman-tree)))))

;; trova un simbolo nella simbolo nella symbol bits table
(defun find-symbol-bits (symbol symbol-bits-table)
  (cdr (assoc symbol symbol-bits-table :test #'equal)))

;; hucodec-encode
(defun hucodec-encode (message huffman-tree)
  (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (labels ((encode-symbol (symbol)
               (let ((bits (find-symbol-bits symbol symbol-bits-table)))
                 (if bits
                     bits
                     (error "Simbolo ~A non presente" symbol))))
             (encode-message (msg)
               (if (null msg)
                   nil
                   (append (encode-symbol (first msg))
                           (encode-message (rest msg))))))
      (encode-message message))))

;; hucodec-encode-file
(defun hucodec-encode-file (filename huffman-tree)
  (with-open-file (stream filename :direction :input)
    (if (null stream)
        (error "errore nel file ~A." filename)
        (let ((content (read-file-content stream)))
          (hucodec-encode content huffman-tree)))))

;; Legge contenuto di un file
(defun read-file-content (stream)
  (labels ((read-chars (acc)
             (let ((char (read-char stream nil nil)))
               (if char
                   (read-chars (cons char acc))
                   (reverse acc)))))
    (read-chars nil)))

;; hucodec-generate-huffman-tree
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (if (null symbols-n-weights)
      (error "Lista Symbol-n-weights vuota.")
      (labels ((make-initial-leaves (pairs)
                 (if (null pairs)
                     nil
                     (cons (make-leaf (car (first pairs)) (cdr (first pairs)))
                           (make-initial-leaves (rest pairs)))))
               
               (sort-by-weight (nodes)
                 (sort nodes #'< :key #'node-weight))
               
               (build-tree (leaves)
                 (if (= 1 (length leaves))
                     (first leaves)  
                     (let* ((sorted-leaves (sort-by-weight leaves))
                            (least1 (first sorted-leaves))
                            (least2 (second sorted-leaves))
                            (rest-leaves (cddr sorted-leaves))
                            (combined (make-node 
                          :weight (+ (node-weight least1) (node-weight least2))
                          :left least1
                          :right least2)))
                       (build-tree (cons combined rest-leaves))))))

        (let ((leaves (make-initial-leaves symbols-n-weights)))
          (if (= 1 (length leaves))
              (let ((single-leaf (first leaves)))
                (make-node 
                 :weight (node-weight single-leaf)
                 :left single-leaf 
                 :right nil)) 
              (build-tree leaves))))))

;; hucodec-generate-symbol-bits-table
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((build-table (node bits acc)
             (cond 

               ((leaf-p node) 
                (cons (cons (leaf-symbol node) bits) acc))
               
               ((and (leaf-p (node-left node)) (null (node-right node)))
                (cons (cons (leaf-symbol (node-left node)) (list 0)) acc))
               
               (t 
                (let ((left-acc (build-table (node-left node)
                 (append bits (list 0)) acc))
                      (right-bits (when (node-right node) 
                        (build-table (node-right node) 
                        (append bits (list 1)) nil))))
                  (append left-acc right-bits))))))
    (build-table huffman-tree nil nil)))


;; hucodec-print-huffman-tree
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  (labels ((print-indented (str level)
    (dotimes (i (* level 2))
        (format t " "))
      (format t "~A~%" str)))
    
  (if (null huffman-tree)
    (print-indented "NIL" indent-level)
      (if (leaf-p huffman-tree)
        (print-indented (format nil "Leaf: ~A (Weight: ~A)" 
            (leaf-symbol huffman-tree) 
            (node-weight huffman-tree))
                        indent-level)
(progn
    (print-indented (format nil "Node (Weight: ~A)" 
                          (node-weight huffman-tree))
                    indent-level)
    (print-indented "Left:" (1+ indent-level))
    (hucodec-print-huffman-tree (node-left huffman-tree) (+ indent-level 2))
    (print-indented "Right:" (1+ indent-level))
    (if (node-right huffman-tree)
      (hucodec-print-huffman-tree (node-right huffman-tree) (+ indent-level 2))
      (print-indented "NIL" (+ indent-level 2))))))
    nil))

