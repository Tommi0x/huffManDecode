%%%% Toloni_Tommaso_914293
%%%% Boschi_Leonardo_914255

%%%% huffman-codes.pl
%%%% sw(Symbol, Weight) - rappresenta un simbolo con il suo peso
%%%% sb(Symbol, Bits) - rappresenta un simbolo con la sua codifica in bit
%%%% leaf(Symbol, Weight) - rappresenta una foglia dell'albero di Huffman
%%%% tree(Left, Right, Weight) - rappresenta un nodo dell'albero di Huffman


%%% GENERATE HUFFMAN TREE %%%

% converte un termine sw(Symbol, Weight) in un termine leaf(Symbol, Weight)
convert_sw_to_leaf(sw(Symbol, Weight), leaf(Symbol, Weight)).

% hucodec_generate_huffman_tree(+SymbolsAndWeights, -HuffmanTree)
% genera un albero huffman a partire da una lista di sw/2
hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
    maplist(convert_sw_to_leaf, SymbolsAndWeights, Leaves),
    build_huffman_tree(Leaves, HuffmanTree).

%funzione di supporto per la generazione dell'albero di huffman
build_huffman_tree([Tree], Tree).
build_huffman_tree(Nodes, Tree) :-
    sort_nodes_by_weight(Nodes, Sorted),
    Sorted = [Node1, Node2 | Rest],
    weight(Node1, W1),
    weight(Node2, W2),
    NewWeight is W1 + W2,
    NewNode = tree(Node1, Node2, NewWeight),
    append(Rest, [NewNode], NewNodes),
    build_huffman_tree(NewNodes, Tree).

%ordinamento dei nodi in base al peso
sort_nodes_by_weight(Nodes, SortedNodes) :-
    map_list_to_pairs(weight, Nodes, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedNodes).

%estrazione del peso di un nodo/foglia
weight(leaf(_, W), W).
weight(tree(_, _, W), W).


%%% GENERATE SYMBOL BITS TABLE %%%

% hucodec_generate_symbol_bits_table(+HuffmanTree, -Table)
% genera una lista di coppie sb(Symbol, Bits) a partire da un albero huffman
hucodec_generate_symbol_bits_table(HuffmanTree, Table) :-
    traverse_tree(HuffmanTree, [], Table).

%attraversamento dell'albero e generazione della tabella
traverse_tree(leaf(Symbol, _), Acc, [sb(Symbol, Bits)]) :-
    Acc = [],
    Bits = [0].
traverse_tree(leaf(Symbol, _), Acc, [sb(Symbol, Bits)]) :-
    Acc \= [],
    Bits = Acc.
traverse_tree(tree(Left, Right, _), Acc, Table) :-
    append(Acc, [0], AccLeft),
    traverse_tree(Left, AccLeft, TableLeft),
    append(Acc, [1], AccRight),
    traverse_tree(Right, AccRight, TableRight),
    append(TableLeft, TableRight, Table).


%%% ENCODE %%%

% hucodec_encode(+Message, +HuffmanTree, -Bits)
% codifica un messaggio a partire da un albero huffman
hucodec_encode(Message, HuffmanTree, Bits) :-
    hucodec_generate_symbol_bits_table(HuffmanTree, Table),
    encode_message(Message, Table, Bits).

%codifica del messaggio
encode_message([], _Table, []).
encode_message([Symbol|Rest], Table, Bits) :-
    member(sb(Symbol, Code), Table),
    encode_message(Rest, Table, BitsRest),
    append(Code, BitsRest, Bits).


%%% DECODE %%%

% hucodec_decode(+Bits, +HuffmanTree, -Message)
% decodifica una sequenza di bit a partire da un albero huffman
hucodec_decode(Bits, HuffmanTree, Message) :-
    HuffmanTree \= leaf(_, _),
    decode_bits(Bits, HuffmanTree, HuffmanTree, Message).
hucodec_decode(Bits, leaf(Symbol, _), Message) :-
    length(Bits, N),                
    length(Message, N),             
    maplist(=(Symbol), Message).     

%decodifica dei bit
decode_bits([], _HuffmanTree, _Current, []).
decode_bits(Bits, HuffmanTree, Current, [Symbol|RestMessage]) :-
    traverse_for_symbol(Bits, Current, Symbol, RemainingBits),
    decode_bits(RemainingBits, HuffmanTree, HuffmanTree, RestMessage).

%attraversamento dell'albero per trovare il simbolo corrispondente
traverse_for_symbol(Bits, leaf(Symbol, _), Symbol, Bits) :- !.
traverse_for_symbol([Bit|RestBits], tree(Left, _, _), Symbol, RemainingBits) :-
    Bit =:= 0,
    traverse_for_symbol(RestBits, Left, Symbol, RemainingBits).
traverse_for_symbol([Bit|RestBits], tree(_, Right, _), Symbol, RemainingBits) :-
    Bit =:= 1,
    traverse_for_symbol(RestBits, Right, Symbol, RemainingBits).


%%% ENCODE FILE %%%

% hucodec_encode_file(+Filename, +HuffmanTree, -Bits)
% codifica un file a partire da un albero huffman
hucodec_encode_file(Filename, HuffmanTree, Bits) :-
    read_file_to_string(Filename, String),
    string_chars(String, Message),
    hucodec_encode(Message, HuffmanTree, Bits).

%lettura del file
read_file_to_string(Filename, String) :-
    open(Filename, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    string_codes(String, Codes).

%lettura dello stream
read_stream_to_codes(Stream, Codes) :-
    read_stream_to_codes(Stream, [], Codes).
read_stream_to_codes(Stream, Acc, Codes) :-
    get_code(Stream, Code),
    process_code(Code, Stream, Acc, Codes).

%formattazione dei codici
process_code(-1, _, Acc, Codes) :-
    reverse(Acc, Codes).
process_code(Code, Stream, Acc, Codes) :-
    Code \= -1,
    read_stream_to_codes(Stream, [Code|Acc], Codes).

%%% PRINT HUFFMAN TREE %%%

% hucodec_print_huffman_tree(+HuffmanTree)
% stampa a video l'albero huffman
hucodec_print_huffman_tree(Tree) :-
    print_tree(Tree, 0).

%formattazione per la stampa dell'albero
print_tree(leaf(Symbol, Weight), Indent) :-
    format('~*cLeaf: ~w (Weight: ~w)~n', [Indent, 32, Symbol, Weight]).
print_tree(tree(Left, Right, Weight), Indent) :-
    format('~*cNode: (Weight: ~w)~n', [Indent, 32, Weight]),
    NewIndent is Indent + 2,
    format('~*cLeft:~n', [Indent, 32]),
    print_tree(Left, NewIndent),
    format('~*cRight:~n', [Indent, 32]),
    print_tree(Right, NewIndent).
