% Carte nel deck
troop(001, 1, 1).
troop(002, 2, 1).
troop(003, 0, 3).
troop(004, 2, 1).

% Stato del gioco
game_state(hand_ia, [001, 001, 001, setting(101), method(201)]).
game_state(field_ia, [3]).  % Nessuna troop giocata
game_state(field_enemy, [1,2,3,4]). % Il nemico ha una carta nello slot 3
game_state(field_setting, setting(102)).
game_state(method_played, false).

% Decidi la mossa migliore, includendo Settings, Methods e Troops
decide_action(BestMoves) :-
    game_state(hand_ia, Hand),
    play_setting(Hand, SettingMove),
    play_method(Hand, MethodMove),
    game_state(field_ia, Field),
    game_state(field_enemy, EnemyField),
    find_defensive_slot(Field, EnemyField, DefensiveSlot),
    (   DefensiveSlot \= none ->
        select_best_move(Hand, DefensiveSlot, TroopMove),
        debug_message("Slot difensivo scelto: ", DefensiveSlot)
    ;   find_offensive_slot(Field, EnemyField, AttackSlot),
        (   AttackSlot \= none ->
            select_best_move(Hand, AttackSlot, TroopMove),
            debug_message("Slot offensivo scelto: ", AttackSlot)
        ;   TroopMove = []  % Nessuna azione possibile
        )
    ),
    append([SettingMove, MethodMove, TroopMove], BestMoves),
    debug_message("Mosse scelte: ", BestMoves).

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

% Trova il miglior slot difensivo disponibile
find_defensive_slot(Field, EnemyField, DefensiveSlot) :-
    findall(Slot, (
        enemy_slot(EnemySlot, EnemyField),
        mirrored_slot(EnemySlot, Slot),
        \+ member(Slot, Field)  % Slot deve essere libero
    ), DefensiveSlots),
    (   DefensiveSlots = [First | _] -> DefensiveSlot = First  % Prende il primo slot valido
    ;   DefensiveSlot = none).

enemy_slot(Index, EnemyField) :- member(Index, EnemyField).

mirrored_slot(4, 1).
mirrored_slot(8, 1).
mirrored_slot(3, 2).
mirrored_slot(7, 2).
mirrored_slot(2, 3).
mirrored_slot(6, 3).
mirrored_slot(1, 4).
mirrored_slot(5, 4).

% Trova il miglior slot offensivo disponibile
find_offensive_slot(Field, EnemyField, AttackSlot) :-
    findall(Slot, (
        mirrored_slot(Slot, AttackSlot),
        \+ member(AttackSlot, Field),
        \+ enemy_slot(AttackSlot, EnemyField)  % Evita di attaccare slot già occupati
    ), OffensiveSlots),
    (   OffensiveSlots = [First | _] -> AttackSlot = First
    ;   AttackSlot = none).

% Seleziona la mossa migliore per le Troop
select_best_move(Hand, Slot, [(Card, ATK, HP, Slot)]) :-
    Slot \= none,
    findall((Card, ATK, HP, Score),
        (member(Card, Hand), troop(Card, ATK, HP), evaluate(Card, ATK, HP, Score)),
        ScoredMoves),
    sort(2, @>=, ScoredMoves, SortedMoves),
    SortedMoves = [(Card, ATK, HP, _) | _],
    debug_message("Giocata troop: ", (Card, ATK, HP, Slot)).

% Valutazione delle carte
evaluate(Troop, ATK, HP, Score) :-
    Score is ATK * 2 + HP.

% Esegui le mosse ed esegui il risultato
esegui(Azioni) :-
    decide_action(Azioni),
    forall(member(Azione, Azioni), stampa_azione(Azione)).

% Stampa il risultato di ogni azione
stampa_azione(("No Move", 0, 0, 0)) :- format('Gioca(No Move, 0, 0, 0)~n').
stampa_azione((Card, "PlaySetting")) :- format('Gioca il nuovo Setting: ~w~n', [Card]).
stampa_azione((Card, "PlayMethod")) :- format('Gioca il nuovo Method: ~w~n', [Card]).
stampa_azione((Card, _, _, Slot)) :- format('Gioca(~w, ~w)~n', [Card, Slot]).

% Funzione di debug
debug_message(Label, Data) :-
    format("DEBUG: ~w ~w~n", [Label, Data]).
