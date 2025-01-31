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
    find_empty_slots(Field, EmptySlots),
    minmax(Hand, EmptySlots, BestMove).

find_empty_slots(Field, EmptySlots) :-
    length(Field, Len),
    MaxSlots is 8,
    Empty is MaxSlots - Len,
    findall(slot, between(1, Empty, slot), EmptySlots).

minmax(Hand, EmptySlots, BestMove) :-
    findall((Card, ATK, HP, Score), (member(Card, Hand), troop(Card, ATK, HP), evaluate(Card, ATK, HP, Score)), ScoredMoves),
    sort(2, @>=, ScoredMoves, SortedMoves),
    select_best_moves(SortedMoves, EmptySlots, BestMove).

select_best_moves(_, [], []).
select_best_moves([(Card, ATK, HP, ) | Rest], [ | SlotsRest], [(Card, ATK, HP) | BestRest]) :-
    select_best_moves(Rest, SlotsRest, BestRest).

% Valutazione punteggio carte
evaluate(Troop, ATK, HP, Score) :-
    Score is ATK * 2 + HP.
