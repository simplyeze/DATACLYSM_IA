:- use_module(library(random)).  % Assicurarsi di avere accesso ai predicati random (SWI-Prolog)

% --- Carte nel deck ---
troop(001, 1, 1).
troop(002, 2, 1).
troop(003, 0, 3).
troop(004, 2, 1).

% --- Stato del gioco ---
game_state(hand_ia, [001, 001, 001, setting(101), method(201)]).
game_state(field_ia, []).
% In questo esempio il player ha già una carta nello slot 1 e nello slot 7
game_state(field_enemy, []).
% Il nemico ha una carta nello slot 2
game_state(field_setting, setting(102)).
game_state(method_played, false).

% --- Predicato principale che decide l'azione migliore ---
decide_action(BestMoves) :-
    game_state(hand_ia, Hand),
    play_setting(Hand, SettingMove),
    play_method(Hand, MethodMove),
    game_state(field_ia, Field),
    game_state(field_enemy, EnemyField),
    % Si controlla se c'è una minaccia non coperta
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

% --- Gestione delle carte Setting e Method ---
play_setting(Hand, [(Setting, "PlaySetting")]) :-
    member(setting(Setting), Hand),
    game_state(field_setting, CurrentSetting),
    Setting \= CurrentSetting, !.
play_setting(_, []).

play_method(Hand, [(Method, "PlayMethod")]) :-
    member(method(Method), Hand),
    game_state(method_played, false), !.
play_method(_, []).

% --- Difesa basata su colonne ---
% Per ogni slot occupato dal nemico, determina la colonna difensiva;
% se nella colonna il player NON ha già una carta, si prova a scegliere uno slot libero.
find_defensive_slot(Field, EnemyField, DefensiveSlot) :-
    member(EnemySlot, EnemyField),
    enemy_defense_column(EnemySlot, DefCol),
    % Se il player ha già una carta in quella colonna, la minaccia è coperta
    \+ defended_by_column(Field, DefCol),
    available_defensive_slot_in_column(Field, DefCol, DefensiveSlot),
    !.
find_defensive_slot(_, _, none).

% --- Difesa Offensiva (scelta pseudocasuale degli slot disponibili) ---
find_offensive_slot(Field, EnemyField, AttackSlot) :-
    % Prima si considerano gli slot in prima linea (1-4)
    findall(Candidate, (
        member(Candidate, [1,2,3,4]),
        \+ member(Candidate, Field),
        \+ enemy_slot(Candidate, EnemyField)
    ), OffensiveFrontSlots),
    (   OffensiveFrontSlots \= [] ->
            random_member(AttackSlot, OffensiveFrontSlots)
    ;   % Se non ci sono slot liberi in prima linea, si controlla la backrow (5-8)
        findall(Candidate, (
            member(Candidate, [5,6,7,8]),
            \+ member(Candidate, Field)
        ), OffensiveBackSlots),
        ( OffensiveBackSlots \= [] ->
                random_member(AttackSlot, OffensiveBackSlots)
        ;   AttackSlot = none
        )
    ).

% --- Predicati ausiliari per la difesa tramite colonne ---

% Mappatura per la difesa: se il nemico è in uno di questi slot,
% la corrispondente colonna difensiva del player è la seguente.
enemy_defense_column(1, 4).
enemy_defense_column(5, 4).
enemy_defense_column(2, 3).
enemy_defense_column(6, 3).
enemy_defense_column(3, 2).
enemy_defense_column(7, 2).
enemy_defense_column(4, 1).
enemy_defense_column(8, 1).

% Mappatura delle colonne per i player: ad esempio, la colonna 3 comprende gli slot 3 e 7.
player_slots_in_column(1, [1,5]).
player_slots_in_column(2, [2,6]).
player_slots_in_column(3, [3,7]).
player_slots_in_column(4, [4,8]).

% Verifica se il player ha già una carta in una data colonna
defended_by_column(Field, Col) :-
    player_slots_in_column(Col, Slots),
    member(Slot, Slots),
    member(Slot, Field),
    !.

% Seleziona uno slot libero in una data colonna per piazzare una troop, scegliendo in maniera pseudocasuale
available_defensive_slot_in_column(Field, Col, Slot) :-
    player_slots_in_column(Col, Slots),
    findall(S, (member(S, Slots), \+ member(S, Field)), AvailableSlots),
    AvailableSlots \= [],
    random_member(Slot, AvailableSlots).

% Predicato che verifica se il nemico occupa un dato slot
enemy_slot(Index, EnemyField) :- member(Index, EnemyField).

% --- Seleziona la mossa migliore per le Troop ---
select_best_move(Hand, Slot, [(Card, ATK, HP, Slot)]) :-
    Slot \= none,
    findall((Card, ATK, HP, Score),
        ( member(Card, Hand),
          troop(Card, ATK, HP),
          evaluate(Card, ATK, HP, Score)
        ),
        ScoredMoves),
    sort(2, @>=, ScoredMoves, SortedMoves),
    SortedMoves = [(Card, ATK, HP, _) | _],
    debug_message("Giocata troop: ", (Card, ATK, HP, Slot)).

% Valutazione della carta (semplice funzione di punteggio)
evaluate(_Troop, ATK, HP, Score) :-
    Score is ATK * 2 + HP.

% --- Esecuzione e stampa delle mosse ---
esegui(Azioni) :-
    decide_action(Azioni),
    forall(member(Azione, Azioni), stampa_azione(Azione)).

stampa_azione(("No Move", 0, 0, 0)) :-
    format('Gioca(No Move, 0, 0, 0)~n').
stampa_azione((Card, "PlaySetting")) :-
    format('Gioca il nuovo Setting: ~w~n', [Card]).
stampa_azione((Card, "PlayMethod")) :-
    format('Gioca il nuovo Method: ~w~n', [Card]).
stampa_azione((Card, _, _, Slot)) :-
    format('Gioca(~w, ~w)~n', [Card, Slot]).

% --- Funzione di debug ---
debug_message(Label, Data) :-
    format("DEBUG: ~w ~w~n", [Label, Data]).
