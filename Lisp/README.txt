Questa libreria Lisp fornisce funzioni per la codifica e decodifica di messaggi utilizzando l'algoritmo di Huffman.

Descrizione strutture implementate:

node: Una struttura che rappresenta un nodo nell'albero di Huffman. Ogni nodo ha i seguenti campi:
- symbol: Il simbolo associato (solo per i nodi foglia).
- weight: Il peso del nodo.
- left: Il figlio sinistro.
- right: Il figlio destro.
- leaf-p: Un flag che indica se il nodo Ã¨ una foglia.

Di seguito sono elencate le principali funzioni disponibili:

hucodec-decode (bits huffman-tree)
Decodifica una sequenza di bit in un messaggio utilizzando un albero di Huffman.

hucodec-encode (message huffman-tree)
Codifica un messaggio in una sequenza di bit utilizzando un albero di Huffman.

hucodec-encode-file (filename huffman-tree)
Codifica il contenuto di un file in una sequenza di bit utilizzando un albero di Huffman.

hucodec-generate-huffman-tree (symbols-n-weights)
Genera un albero di Huffman da una lista di coppie (simbolo . peso).

hucodec-generate-symbol-bits-table (huffman-tree)
Genera una tabella che mappa i simboli alle loro sequenze di bit utilizzando un albero di Huffman.

hucodec-print-huffman-tree (huffman-tree &optional indent-level)
Stampa una rappresentazione dell'albero di Huffman con un livello di indentazione opzionale.