Questa libreria Prolog fornisce funzioni per la codifica e decodifica di messaggi utilizzando l'algoritmo di Huffman.

Descrizione strutture implementate:
- sw(Symbol, Weight): Rappresenta un simbolo con il suo peso.
- sb(Symbol, Bits): Rappresenta un simbolo con la sua codifica in bit.
- leaf(Symbol, Weight): Rappresenta una foglia dell'albero di Huffman.
- tree(Left, Right, Weight): Rappresenta un nodo dell'albero di Huffman.

Di seguito sono elencate le principali funzioni disponibili:

hucodec_decode(+Bits, +HuffmanTree, -Message)
Decodifica una sequenza di bit in un messaggio utilizzando un albero di Huffman.

hucodec_encode(+Message, +HuffmanTree, -Bits)
Codifica un messaggio in una sequenza di bit utilizzando un albero di Huffman.

hucodec_encode_file(+Filename, +HuffmanTree, -Bits)
Codifica il contenuto di un file in una sequenza di bit utilizzando un albero di Huffman.

hucodec_generate_huffman_tree(+SymbolsAndWeights, -HuffmanTree)
Genera un albero di Huffman da una lista di simboli e pesi.

hucodec_generate_symbol_bits_table(+HuffmanTree, -SymbolBitsTable)
Genera una tabella di simboli e sequenze di bit da un albero di Huffman.

hucodec_print_huffman_tree(+HuffmanTree)
Stampa una rappresentazione dell'albero di Huffman.