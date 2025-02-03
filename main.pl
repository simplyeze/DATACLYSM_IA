% Carte nel deck
troop(001, 1, 1).
troop(002, 2, 1).
troop(003, 0, 3).
troop(004, 2, 1).

% Stato del gioco
game_state(hand_ia, [001, 001, 001, setting(101), method(201)]).
game_state(field_ia, [1]).
game_state(field_enemy, [3, 4]). % Truppe nemiche sul campo
game_state(field_setting, setting(102)).
game_state(method_played, false).

% Decidi la mossa migliore, includendo Settings, Methods e Troops
decide_action(BestMoves) :-
    game_state(hand_ia, Hand),
    play_setting(Hand, SettingMove),
    play_method(Hand, MethodMove),
    game_state(field_ia, Field),
    game_state(field_enemy, EnemyField),
    find_defensive_slots(Field, EnemyField, DefensiveSlots),
    (   DefensiveSlots \= [] -> select_best_move(Hand, DefensiveSlots, TroopMove)
    ;   find_strategic_slots(Field, EnemyField, AttackSlots),
        (   AttackSlots \= [] -> select_best_move(Hand, AttackSlots, TroopMove)
        ;   TroopMove = []  % Se non ci sono slot, non giocare alcuna troop
        )
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

% Trova gli slot difensivi disponibili per proteggere da attacchi diretti
find_defensive_slots(Field, EnemyField, DefensiveSlots) :-
    MaxSlots is 8,
    findall(Index, (
        between(1, MaxSlots, Index),
        \+ member(Index, Field),  % Slot libero
        column(Index, Column),
        \+ (member(Protected, Column), member(Protected, Field))  % Nessuna difesa esistente
    ), DefensiveSlots).

% Definisce le colonne sul campo di battaglia
column(1, [1, 5]).
column(2, [2, 6]).
column(3, [3, 7]).
column(4, [4, 8]).
column(5, [1, 5]).
column(6, [2, 6]).
column(7, [3, 7]).
column(8, [4, 8]).

% Trova gli slot strategici disponibili per attacco
find_strategic_slots(Field, EnemyField, EmptySlots) :-
    MaxSlots is 8,
    findall(Index, (between(1, MaxSlots, Index), \+ member(Index, Field)), AllEmptySlots),
    prioritize_slots(AllEmptySlots, [], EmptySlots).

% Determina gli slot critici basati sulla posizione delle truppe nemiche
find_critical_slots(Field, EnemyField, CriticalSlots) :-
    findall(Slot, (
        member(EnemySlot, EnemyField),
        target_slots(EnemySlot, TargetedSlots),
        member(Slot, TargetedSlots),
        \+ member(Slot, Field)
    ), Slots),
    sort(Slots, CriticalSlots).

% Determina gli slot bersagliati da un dato slot nemico
target_slots(1,  [4, 8]).
target_slots(2,  [3, 7]).
target_slots(3,  [2, 6]).
target_slots(4,  [1, 5]).
target_slots(5,  [4, 8]).
target_slots(6,  [3, 7]).
target_slots(7,  [2, 6]).
target_slots(8,  [1, 5]).

% Prioritizza gli slot disponibili dando precedenza a quelli critici
prioritize_slots(AllEmpty, Critical, Prioritized) :-
    intersection(AllEmpty, Critical, Important),
    subtract(AllEmpty, Critical, NonCritical),
    append(Important, NonCritical, Prioritized).

% Seleziona la mossa migliore per le Troop solo se ci sono slot disponibili
select_best_move(Hand, EmptySlots, [(Card, ATK, HP, Slot)]) :-
    EmptySlots \= [],  % Verifica che ci siano slot disponibili
    findall((Card, ATK, HP, Score),
        (member(Card, Hand), troop(Card, ATK, HP), evaluate(Card, ATK, HP, Score)),
        ScoredMoves),
    sort(2, @>=, ScoredMoves, SortedMoves),
    SortedMoves = [(Card, ATK, HP, _) | _],
    EmptySlots = [Slot | _].

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
