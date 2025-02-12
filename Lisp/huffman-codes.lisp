;;; huffman-codes.lisp
;;; Autori: [Inserisci qui i nomi dei membri del gruppo]
;;; Descrizione: Implementazione di codifica/decodifica Huffman

;;;; DEFINIZIONE DELLA STRUTTURA DELL'ALBERO
(defstruct huff-node
  symbol   ;; per foglie: il simbolo, per nodi interni: lista dei simboli uniti
  weight   ;; peso (frequenza)
  left     ;; sottoalbero sinistro
  right)   ;; sottoalbero destro

;;;; PREDICATI DI SUPPORTO
(defun leaf-p (node)
  "Verifica se NODE è una foglia (non ha figli)."
  (and (null (huff-node-left node))
       (null (huff-node-right node))))

(defun leaf-symbol (node)
  "Restituisce il simbolo memorizzato in una foglia."
  (huff-node-symbol node))

(defun choose-branch (bit branch)
  "Data un'informazione BIT (0 o 1) e un nodo BRANCH, seleziona il ramo corrispondente."
  (cond ((= bit 0) (huff-node-left branch))
        ((= bit 1) (huff-node-right branch))
        (t (error "Bad bit ~D." bit))))

;;;; FUNZIONE DI DECODIFICA
(defun hucodec-decode (bits huffman-tree)
  "Decodifica una lista di BIT usando l'albero di Huffman HUFFMAN-TREE."
  (labels ((decode-1 (bits current-branch)
             (if (null bits)
                 nil
                 (let ((next-branch (choose-branch (first bits) current-branch)))
                   (if (leaf-p next-branch)
                       (cons (leaf-symbol next-branch)
                             (decode-1 (rest bits) huffman-tree))
                       (decode-1 (rest bits) next-branch))))))
    (decode-1 bits huffman-tree)))

;;;; COSTRUZIONE DELL'ALBERO DI HUFFMAN
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  "Data una lista di coppie (simbolo . peso), genera l'albero di Huffman."
  (let ((nodes (mapcar (lambda (sw)
                         (make-huff-node :symbol (car sw)
                                         :weight (cdr sw)
                                         :left nil :right nil))
                       symbols-n-weights)))
    (labels ((combine (node1 node2)
               (make-huff-node :symbol (list (huff-node-symbol node1)
                                              (huff-node-symbol node2))
                               :weight (+ (huff-node-weight node1)
                                          (huff-node-weight node2))
                               :left node1 :right node2)))
      (loop while (> (length nodes) 1) do
           (setf nodes (sort nodes #'< :key #'huff-node-weight))
           (let* ((node1 (first nodes))
                  (node2 (second nodes))
                  (new-node (combine node1 node2)))
             (setf nodes (cons new-node (nthcdr 2 nodes)))))
      (first nodes))))

;;;; GENERAZIONE DELLA TABELLA SIMBOLO -> BIT STRING
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  "Data l'albero HUFFMAN-TREE, genera una lista di coppie (simbolo . bits)."
  (labels ((traverse (node prefix)
             (if (leaf-p node)
                 (list (cons (huff-node-symbol node) prefix))
                 (append (traverse (huff-node-left node) (append prefix '(0)))
                         (traverse (huff-node-right node) (append prefix '(1)))))))
    (traverse huffman-tree '())))

;;;; FUNZIONE DI CODIFICA
(defun hucodec-encode (message huffman-tree)
  "Codifica MESSAGE (lista di simboli) usando HUFFMAN-TREE, restituendo una lista di 0 e 1."
  (let ((table (hucodec-generate-symbol-bits-table huffman-tree)))
    (apply #'append
           (mapcar (lambda (sym)
                     (let ((entry (assoc sym table)))
                       (if entry
                           (cdr entry)
                           (error "Symbol ~A not found in Huffman tree." sym))))
                   message))))

;;;; CODIFICA DI UN FILE
(defun hucodec-encode-file (filename huffman-tree)
  "Legge il file specificato da FILENAME e restituisce la codifica in bit del suo contenuto."
  (with-open-file (in filename :direction :input)
    (let ((contents (read-line in nil)))
      (unless contents
        (error "File ~A is empty or could not be read." filename))
      ;; Per semplicità si assume che il messaggio sia una lista di caratteri (convertendo la stringa in lista)
      (let ((message (coerce (string-upcase contents) 'list)))
        (hucodec-encode message huffman-tree)))))

;;;; STAMPA DELL'ALBERO (DEBUGGING)
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  "Stampa l'albero di Huffman con un'indentazione per visualizzare la struttura."
  (let ((indent (make-string indent-level :initial-element #\Space)))
    (if (leaf-p huffman-tree)
        (format t "~aLeaf: ~a (weight: ~a)~%" indent (huff-node-symbol huffman-tree) (huff-node-weight huffman-tree))
        (progn
          (format t "~aNode: (weight: ~a)~%" indent (huff-node-weight huffman-tree))
          (format t "~aLeft:~%" indent)
          (hucodec-print-huffman-tree (huff-node-left huffman-tree) (+ indent-level 2))
          (format t "~aRight:~%" indent)
          (hucodec-print-huffman-tree (huff-node-right huffman-tree) (+ indent-level 2))))))

;;;; ESEMPIO DI UTILIZZO
;; (defparameter *symbols-n-weights* '((A . 8) (B . 3) (C . 1) (D . 1)
;;                                      (E . 1) (F . 1) (G . 1) (H . 1)))
;; (defparameter *huffman-tree* (hucodec-generate-huffman-tree *symbols-n-weights*))
;; (defparameter *message* '(B A C))
;; (hucodec-print-huffman-tree *huffman-tree*)
;; (defparameter *encoded* (hucodec-encode *message* *huffman-tree*))
;; (format t "Encoded message: ~a~%" *encoded*)
;; (format t "Decoded message: ~a~%" (hucodec-decode *encoded* *huffman-tree*))
