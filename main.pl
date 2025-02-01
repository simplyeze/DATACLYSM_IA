% Carte nel deck
troop(001, 1, 1).
troop(002, 2, 1).
troop(003, 0, 3).
troop(004, 2, 1).

% Stato del gioco
game_state(hand_ia, [001, 001, 001]).
game_state(field_ia, [1, 2, 3, 4, 5]).

% Decidi la mossa migliore
decide_action(BestMove) :-
    game_state(hand_ia, Hand),
    game_state(field_ia, Field),
    find_strategic_slots(Field, EmptySlots),
    (   EmptySlots \= []  % Se ci sono slot vuoti, si può fare una mossa
    ->  format('Debug: Slot vuoti = ~w~n', [EmptySlots]),
        select_best_move(Hand, EmptySlots, BestMove)  % Se ci sono mosse valide
    ;   BestMove = [("No Move", 0, 0, 0)]  % Altrimenti, nessuna mossa
    ).

% Trova gli slot strategici disponibili
find_strategic_slots(Field, EmptySlots) :-
    MaxSlots is 8,  % Numero massimo di slot
    format('Debug: Slot totali disponibili = ~w~n', [MaxSlots]),
    findall(Index, (between(1, MaxSlots, Index), \+ member(Index, Field)), EmptySlots),
    format('Debug: Slot vuoti = ~w~n', [EmptySlots]).

% Seleziona la mossa migliore (basata solo sul punteggio della carta)
select_best_move(Hand, EmptySlots, BestMove) :-
    findall((Card, ATK, HP, Score),
        (member(Card, Hand), troop(Card, ATK, HP), evaluate(Card, ATK, HP, Score)),
        ScoredMoves),
    format('Debug: Carte nella mano con punteggio = ~w~n', [ScoredMoves]),
    % Ordina per punteggio decrescente
    sort(2, @>=, ScoredMoves, SortedMoves),
    format('Debug: Carte ordinate = ~w~n', [SortedMoves]),
    % Assegna la mossa migliore
    SortedMoves = [(Card, ATK, HP, _) | _],
    EmptySlots = [Slot | _],  % Seleziona il primo slot disponibile
    BestMove = [(Card, ATK, HP, Slot)],
    format('Debug: Mossa selezionata = ~w~n', [BestMove]).

% Valutazione delle carte
evaluate(Troop, ATK, HP, Score) :-
    Score is ATK * 2 + HP,
    format('Debug: Valutazione carta ~w con ATK ~w, HP ~w, Punteggio ~w~n', [Troop, ATK, HP, Score]).

% Esegui la mossa e mostra il risultato
esegui(Azione) :-
    decide_action(Azione),
    (   Azione = [("No Move", 0, 0, 0)]  % Se non c'è una mossa valida
    ->  format('Gioca(No Move, 0, 0, 0)~n')
    ;   Azione = [(Card, _, _, Slot)]  % Se c'è una mossa valida
    ->  format('Gioca(~w, ~w)~n', [Card, Slot])
    ).
