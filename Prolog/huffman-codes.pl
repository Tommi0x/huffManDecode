%% huffman-codes.pl
%% Autori: [Inserisci qui i nomi dei membri del gruppo]
%% Descrizione: Implementazione dei predicati per codifica/decodifica Huffman
%%
%% Rappresentazione:
%%   - Una foglia viene rappresentata come:  leaf(Symbol, Weight).
%%   - Un nodo interno viene rappresentato come:  tree(Left, Right, Weight).
%%   - Le coppie simbolo-peso sono espresse come:  sw(Symbol, Weight).
%%   - Le coppie simbolo-bits sono espresse come:  sb(Symbol, Bits).

%%%%% COSTRUZIONE DELL'ALBERO DI HUFFMAN %%%%%

% Converte una coppia sw in una foglia.
convert_sw_to_leaf(sw(Symbol, Weight), leaf(Symbol, Weight)).

% Genera l'albero di Huffman da una lista di coppie sw/2.
hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
    maplist(convert_sw_to_leaf, SymbolsAndWeights, Leaves),
    build_huffman_tree(Leaves, HuffmanTree).

% Costruisce ricorsivamente l'albero combinando i due nodi con peso minore.
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

% Ordina la lista di nodi in base al peso, preservando duplicati.
sort_nodes_by_weight(Nodes, SortedNodes) :-
    % Associa ad ogni nodo il proprio peso
    map_list_to_pairs(weight, Nodes, Pairs),
    % keysort è stabile e non elimina duplicati
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedNodes).

weight(leaf(_, W), W).
weight(tree(_, _, W), W).

%%%%% GENERAZIONE DELLA TABELLA SIMBOLO -> BIT STRING %%%%%

% Genera la tabella sb(Symbol, Bits) attraversando l'albero.
hucodec_generate_symbol_bits_table(HuffmanTree, Table) :-
    traverse_tree(HuffmanTree, [], Table).

traverse_tree(leaf(Symbol, _), Acc, [sb(Symbol, Bits)]) :-
    ( Acc = [] -> Bits = [0]
    ; Bits = Acc
    ).
traverse_tree(tree(Left, Right, _), Acc, Table) :-
    append(Acc, [0], AccLeft),
    traverse_tree(Left, AccLeft, TableLeft),
    append(Acc, [1], AccRight),
    traverse_tree(Right, AccRight, TableRight),
    append(TableLeft, TableRight, Table).

%%%%% CODIFICA %%%%%

% hucodec_encode(+Message, +HuffmanTree, -Bits)
% DATA: Message è una lista di simboli (ad es. [a,b,c,...])
hucodec_encode(Message, HuffmanTree, Bits) :-
    hucodec_generate_symbol_bits_table(HuffmanTree, Table),
    encode_message(Message, Table, Bits).

encode_message([], _Table, []).
encode_message([Symbol|Rest], Table, Bits) :-
    member(sb(Symbol, Code), Table),
    encode_message(Rest, Table, BitsRest),
    append(Code, BitsRest, Bits).

%%%%% DECODIFICA %%%%%

% hucodec_decode(+Bits, +HuffmanTree, -Message)
hucodec_decode(Bits, HuffmanTree, Message) :-
    HuffmanTree \= leaf(_, _),
    decode_bits(Bits, HuffmanTree, HuffmanTree, Message).

hucodec_decode(Bits, leaf(Symbol, _), Message) :-
    length(Bits, N),                % Determine the number of bits
    length(Message, N),             % Ensure Message has the same length
    maplist(=(Symbol), Message).     % Assign the Symbol to each position in Message

decode_bits([], _HuffmanTree, _Current, []).
decode_bits(Bits, HuffmanTree, Current, [Symbol|RestMessage]) :-
    traverse_for_symbol(Bits, Current, Symbol, RemainingBits),
    decode_bits(RemainingBits, HuffmanTree, HuffmanTree, RestMessage).

% traverse_for_symbol(+Bits, +CurrentNode, -Symbol, -RemainingBits)
traverse_for_symbol(Bits, leaf(Symbol, _), Symbol, Bits) :- !.
traverse_for_symbol([Bit|RestBits], tree(Left, Right, _), Symbol, RemainingBits) :-
    ( Bit =:= 0 ->
        traverse_for_symbol(RestBits, Left, Symbol, RemainingBits)
    ; Bit =:= 1 ->
        traverse_for_symbol(RestBits, Right, Symbol, RemainingBits)
    ).

%%%%% CODIFICA DI UN FILE %%%%%

% hucodec_encode_file(+Filename, +HuffmanTree, -Bits)
hucodec_encode_file(Filename, HuffmanTree, Bits) :-
    read_file_to_string(Filename, String),
    string_chars(String, Message),
    hucodec_encode(Message, HuffmanTree, Bits).

% Legge il contenuto di un file in una stringa.
read_file_to_string(Filename, String) :-
    open(Filename, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    string_codes(String, Codes).

read_stream_to_codes(Stream, Codes) :-
    read_stream_to_codes(Stream, [], Codes).

read_stream_to_codes(Stream, Acc, Codes) :-
    get_code(Stream, Code),
    ( Code = -1 -> reverse(Acc, Codes)
    ; read_stream_to_codes(Stream, [Code|Acc], Codes)
    ).

%%%%% STAMPA DELL'ALBERO %%%%%

% hucodec_print_huffman_tree(+HuffmanTree)
hucodec_print_huffman_tree(Tree) :-
    print_tree(Tree, 0).

print_tree(leaf(Symbol, Weight), Indent) :-
    format('~*cLeaf: ~w (Weight: ~w)~n', [Indent, 32, Symbol, Weight]).
print_tree(tree(Left, Right, Weight), Indent) :-
    format('~*cNode: (Weight: ~w)~n', [Indent, 32, Weight]),
    NewIndent is Indent + 2,
    format('~*cLeft:~n', [Indent, 32]),
    print_tree(Left, NewIndent),
    format('~*cRight:~n', [Indent, 32]),
    print_tree(Right, NewIndent).


%%%%%% TEST %%%%%%
% Test case 1
