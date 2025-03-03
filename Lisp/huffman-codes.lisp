;;;; huffman-codes.lisp - Implementation of Huffman encoding/decoding library


;; Define the structure for a node in the Huffman tree
(defstruct node
  (symbol nil)     ; Symbol (only for leaf nodes)
  (weight 0)       ; Weight of the node
  (left nil)       ; Left child
  (right nil)      ; Right child
  (leaf-p nil))    ; Flag to identify leaf nodes

;; Constructor for leaf nodes
(defun make-leaf (symbol weight)
  (make-node :symbol symbol :weight weight :leaf-p t))

;; Function to check if a node is a leaf
(defun leaf-p (node)
  (node-leaf-p node))

;; Function to get the symbol of a leaf node
(defun leaf-symbol (node)
  (node-symbol node))

;; Function to get the symbols in a node (all symbols in the subtree)
(defun node-symbols (node)
  (if (leaf-p node)
      (list (leaf-symbol node))
      (append (node-symbols (node-left node))
              (node-symbols (node-right node)))))

;; Choose the appropriate branch based on the bit
(defun choose-branch (bit branch)
  (cond ((= 0 bit) (node-left branch))
        ((= 1 bit) (if (node-right branch)
                       (node-right branch)
                       (error "Invalid bit 1 for a tree with no right branch.")))
        (t (error "Bad bit ~D." bit))))

;;; Core functions

;; Huffman decoding implementation
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
    (if (null bits)
        nil
        (decode-1 bits huffman-tree))))

;; Helper function to find a symbol in the symbol-bits table
(defun find-symbol-bits (symbol symbol-bits-table)
  (cdr (assoc symbol symbol-bits-table :test #'equal)))

;; Huffman encoding implementation
(defun hucodec-encode (message huffman-tree)
  (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (labels ((encode-symbol (symbol)
               (let ((bits (find-symbol-bits symbol symbol-bits-table)))
                 (if bits
                     bits
                     (error "Symbol ~A not found in the Huffman tree." symbol))))
             (encode-message (msg)
               (if (null msg)
                   nil
                   (append (encode-symbol (first msg))
                           (encode-message (rest msg))))))
      (encode-message message))))

;; Read a file and encode its contents
(defun hucodec-encode-file (filename huffman-tree)
  (with-open-file (stream filename :direction :input)
    (if (null stream)
        (error "Could not open file ~A." filename)
        (let ((content (read-file-content stream)))
          (hucodec-encode content huffman-tree)))))

;; Helper function to read file content
(defun read-file-content (stream)
  (labels ((read-chars (acc)
             (let ((char (read-char stream nil nil)))
               (if char
                   (read-chars (cons char acc))
                   (reverse acc)))))
    (read-chars nil)))

;; Modified function to generate a Huffman tree from a list of symbols and weights
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (if (null symbols-n-weights)
      (error "Empty symbols-n-weights list.")
      (labels ((make-initial-leaves (pairs)
                 (if (null pairs)
                     nil
                     (cons (make-leaf (car (first pairs)) (cdr (first pairs)))
                           (make-initial-leaves (rest pairs)))))
               
               (sort-by-weight (nodes)
                 (sort nodes #'< :key #'node-weight))
               
               (build-tree (leaves)
                 (if (= 1 (length leaves))
                     ;; For a single leaf, create a proper tree with left and right branches
                     (let ((single-leaf (first leaves)))
                       (make-node 
                        :weight (node-weight single-leaf)
                        :left single-leaf
                        :right (make-leaf 'dummy 0))) ; Dummy right leaf that won't be used
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
          ;; Special case: if only one symbol, create a proper tree structure
          (if (= 1 (length leaves))
              (let ((single-leaf (first leaves)))
                (make-node 
                 :weight (node-weight single-leaf)
                 :left single-leaf 
                 :right nil)) ; This way, choosing branch 0 will lead to the symbol
              (build-tree leaves))))))

;; Modified function to generate a table mapping symbols to their bit sequences
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((build-table (node bits acc)
             (if (leaf-p node)
                 (cons (cons (leaf-symbol node) bits) acc)
                 (let ((left-acc (build-table (node-left node) (append bits (list 0)) acc))
                       (right-bits (when (node-right node) ; Only build right branch if it exists
                                     (build-table (node-right node) (append bits (list 1)) nil))))
                   (append left-acc right-bits)))))
    (build-table huffman-tree nil nil)))


;; Modified print function to handle NIL branches
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

;;; Example usage:
#|
(defparameter *sample-weights* '((#\A . 8) (#\B . 3) (#\C . 1) (#\D . 1) (#\E . 1) (#\F . 1) (#\G . 1) (#\H . 1)))
(defparameter *ht* (hucodec-generate-huffman-tree *sample-weights*))
(defparameter *message* '(#\B #\A #\C))
(defparameter *encoded* (hucodec-encode *message* *ht*))
(defparameter *decoded* (hucodec-decode *encoded* *ht*))
(equal *message* *decoded*)  ; Should return T
(hucodec-print-huffman-tree *ht*)  ; Print the tree for visualization
|#

