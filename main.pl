% Carte nel deck
troop(sandbag, 1, 1).
troop(ranger, 2, 1).
troop(armorer, 0, 3).
troop(dragon, 2, 1).

% Stato del gioco
game_state(hand_ia, []).
game_state(field_ia, []).

decide_action(BestMove) :-
    game_state(hand_ia, Hand),
    game_state(field_ia, Field),
    find_strategic_slots(Field, EmptySlots),
    minmax(Hand, EmptySlots, BestMove).

find_strategic_slots(Field, SortedSlots) :-
    length(Field, Len),
    MaxSlots is 8,
							
    findall(Index, (between(1, MaxSlots, Index), \+ member(Index, Field)), EmptySlots),
    sort_slots_by_priority(EmptySlots, SortedSlots).

sort_slots_by_priority(EmptySlots, SortedSlots) :-
    findall((Slot, Score), (member(Slot, EmptySlots), slot_priority(Slot, Score)), ScoredSlots),
    sort(2, @>=, ScoredSlots, SortedPairs),
    extract_slots(SortedPairs, SortedSlots).

extract_slots([], []).
extract_slots([(Slot, _) | Rest], [Slot | SortedRest]) :-
    extract_slots(Rest, SortedRest).

slot_priority(Slot, Score) :-
    (Slot = 1 ; Slot = 8) -> Score is 3 ;  % Preferisci i lati per protezione
    (Slot = 4 ; Slot = 5) -> Score is 2 ;  % Preferisci il centro per attacco bilanciato
    Score is 1.  % Altri slot meno prioritari

minmax(Hand, EmptySlots, BestMove) :-
    findall((Card, ATK, HP, Score), (member(Card, Hand), troop(Card, ATK, HP), evaluate(Card, ATK, HP, Score)), ScoredMoves),
    sort(2, @>=, ScoredMoves, SortedMoves),
    select_best_moves(SortedMoves, EmptySlots, BestMove).

select_best_moves(_, [], []).
select_best_moves([(Card, ATK, HP, _) | Rest], [Slot | SlotsRest], [(Card, ATK, HP, Slot) | BestRest]) :-
    select_best_moves(Rest, SlotsRest, BestRest).

% Valutazione punteggio carte
evaluate(Troop, ATK, HP, Score) :-
    Score is ATK * 2 + HP.
