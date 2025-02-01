% Carte nel deck
troop(001, 1, 1).
troop(002, 2, 1).
troop(003, 0, 3).
troop(004, 2, 1).

% Stato del gioco
game_state(hand_ia, [001, 001, 001, setting(101), method(201)]).
game_state(field_ia, [1, 2, 3, 4, 5]).
game_state(field_setting, setting(102)).  % Setting attuale sul campo
game_state(method_played, false).  % Indica se è già stato giocato un Method

% Decidi la mossa migliore, includendo Settings, Methods e Troops
decide_action(BestMoves) :-
    game_state(hand_ia, Hand),
    play_setting(Hand, SettingMove),   % Verifica se può giocare un Setting
    play_method(Hand, MethodMove),     % Verifica se può giocare un Method
    game_state(field_ia, Field),
    find_strategic_slots(Field, EmptySlots),
    (   EmptySlots \= []  
    ->  select_best_move(Hand, EmptySlots, TroopMove)  
    ;   TroopMove = []
    ),
    append([SettingMove, MethodMove, TroopMove], BestMoves).

% Gioca un Setting se disponibile e diverso da quello sul campo
play_setting(Hand, [(Setting, "PlaySetting")]) :-
    member(setting(Setting), Hand),
    game_state(field_setting, CurrentSetting),
    Setting \= CurrentSetting, !.
play_setting(_, []).

% Gioca un Method se disponibile e non ne ha ancora giocato uno
play_method(Hand, [(Method, "PlayMethod")]) :-
    member(method(Method), Hand),
    game_state(method_played, false), !.
play_method(_, []).

% Trova gli slot strategici disponibili
find_strategic_slots(Field, EmptySlots) :-
    MaxSlots is 8,  
    findall(Index, (between(1, MaxSlots, Index), \+ member(Index, Field)), EmptySlots).

% Seleziona la mossa migliore per le Troop
select_best_move(Hand, EmptySlots, [(Card, ATK, HP, Slot)]) :-
    findall((Card, ATK, HP, Score),
        (member(Card, Hand), troop(Card, ATK, HP), evaluate(Card, ATK, HP, Score)),
        ScoredMoves),
    sort(2, @>=, ScoredMoves, SortedMoves),
    SortedMoves = [(Card, ATK, HP, _) | _],
    EmptySlots = [Slot | _].

% Valutazione delle carte
evaluate(Troop, ATK, HP, Score) :-
    Score is ATK * 2 + HP.

% Esegui le mosse e mostra il risultato
esegui(Azioni) :-
    decide_action(Azioni),
    forall(member(Azione, Azioni), stampa_azione(Azione)).

% Stampa il risultato di ogni azione
stampa_azione(("No Move", 0, 0, 0)) :- format('Gioca(No Move, 0, 0, 0)~n').
stampa_azione((Card, "PlaySetting")) :- format('Gioca il nuovo Setting: ~w~n', [Card]).
stampa_azione((Card, "PlayMethod")) :- format('Gioca il nuovo Method: ~w~n', [Card]).
stampa_azione((Card, _, _, Slot)) :- format('Gioca(~w, ~w)~n', [Card, Slot]).

    ->  format('Gioca(No Move, 0, 0, 0)~n')
    ;   Azione = [(Card, _, _, Slot)]  % Se c'è una mossa valida
    ->  format('Gioca(~w, ~w)~n', [Card, Slot])
    ).
