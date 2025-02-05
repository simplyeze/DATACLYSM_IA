:- use_module(library(random)).  % Per l'uso di random_member/2

% --- Carte nel deck (troop/4: Id, ATK, HP, EFFECT_ID) ---
troop(001, 1, 1, 0).
troop(002, 0, 1, 1).  % Questa carta ha un effetto speciale (EFFECT_ID = 1)
troop(003, 0, 3, 0).
troop(004, 0, 6, 0).
% Ad esempio, una carta extra per il nemico
troop(005, 5, 5, 0).

% --- Stato del gioco ---
% I campi (sia per l'IA che per il nemico) usano la struttura slot(NumeroSlot, IdCarta)
game_state(hand_ia, [001, 002, 001, setting(101), method(201)]).
game_state(field_ia, [slot(1, 001)]).

% Anche il nemico ora usa ID numerici (ad es. 003)
game_state(field_enemy, [slot(2, 001)]).
game_state(field_setting, setting(102)).
game_state(method_played, false).

% --- Predicato principale che decide l'azione migliore ---
decide_action(BestMoves) :-
    game_state(hand_ia, Hand),
    play_setting(Hand, SettingMove),
    play_method(Hand, MethodMove),
    game_state(field_ia, Field),
    game_state(field_enemy, EnemyField),
    % Determina lo slot in cui piazzare la carta, in base alla strategia difensiva o offensiva
    (   find_defensive_slot(Field, EnemyField, ChosenSlot),
        ChosenSlot \= none
    ->  SelectedSlot = ChosenSlot,
        debug_message("Slot difensivo scelto: ", SelectedSlot)
    ;   find_offensive_slot(Field, EnemyField, ChosenSlot),
        ChosenSlot \= none
    ->  SelectedSlot = ChosenSlot,
        debug_message("Slot offensivo scelto: ", SelectedSlot)
    ;   SelectedSlot = none
    ),
    (   SelectedSlot \= none
    ->  % Passa anche EnemyField all'euristica, così da poter valutare il bonus in base al nemico attaccato
        select_best_move(Hand, SelectedSlot, EnemyField, TroopMove)
    ;   TroopMove = []
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
    member(slot(EnemySlot, _), EnemyField),
    enemy_defense_column(EnemySlot, DefCol),
    \+ defended_by_column(Field, DefCol),
    available_defensive_slot_in_column(Field, DefCol, DefensiveSlot),
    !.
find_defensive_slot(_, _, none).

% --- Difesa Offensiva (scelta pseudocasuale degli slot disponibili) ---
find_offensive_slot(Field, EnemyField, AttackSlot) :-
    % Prima si considerano gli slot in prima linea (1-4)
    findall(Candidate, (
        member(Candidate, [1,2,3,4]),
        \+ occupied_slot(Field, Candidate),
        \+ enemy_slot(EnemyField, Candidate)
    ), OffensiveFrontSlots),
    (   OffensiveFrontSlots \= [] ->
            random_member(AttackSlot, OffensiveFrontSlots)
    ;   % Se non ci sono slot liberi in prima linea, si controlla la backrow (5-8)
        findall(Candidate, (
            member(Candidate, [5,6,7,8]),
            \+ occupied_slot(Field, Candidate)
        ), OffensiveBackSlots),
        (   OffensiveBackSlots \= [] ->
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

% Mappatura delle colonne per il player: ad esempio, la colonna 3 comprende gli slot 3 e 7.
player_slots_in_column(1, [1,5]).
player_slots_in_column(2, [2,6]).
player_slots_in_column(3, [3,7]).
player_slots_in_column(4, [4,8]).

% Verifica se il player ha già una carta in una data colonna
defended_by_column(Field, Col) :-
    player_slots_in_column(Col, Slots),
    member(Slot, Slots),
    occupied_slot(Field, Slot),
    !.

% Seleziona uno slot libero in una data colonna per piazzare una troop, scegliendo in maniera pseudocasuale
available_defensive_slot_in_column(Field, Col, Slot) :-
    player_slots_in_column(Col, Slots),
    findall(S, (member(S, Slots), \+ occupied_slot(Field, S)), AvailableSlots),
    AvailableSlots \= [],
    random_member(Slot, AvailableSlots).

% --- Predicato ausiliario per verificare se uno slot è occupato nel Field ---
occupied_slot(Field, Slot) :-
    member(slot(Slot, _), Field).

% --- Predicato per verificare se il nemico occupa un dato slot ---
enemy_slot(EnemyField, Slot) :-
    member(slot(Slot, _), EnemyField).

% --- Mappatura classica per i mirrored_slot ---
mirrored_slot(4, 1).
mirrored_slot(8, 1).
mirrored_slot(3, 2).
mirrored_slot(7, 2).
mirrored_slot(2, 3).
mirrored_slot(6, 3).
mirrored_slot(1, 4).
mirrored_slot(5, 4).

% Predicato per determinare lo slot nemico attaccato, dato lo slot IA.
mirrored_enemy_slot(IA_Slot, EnemySlot) :-
    mirrored_slot(EnemySlot, IA_Slot).

% --- Seleziona la mossa migliore per le Troop ---
% Nota: il termine che raccoglie i dati ora è esplicitamente una quadrupla,
% ad esempio: tuple(Card, ATK, HP, Score)
select_best_move(Hand, IA_Slot, EnemyField, [(Card, ATK, HP, IA_Slot)]) :-
    IA_Slot \= none,
    findall(tuple(Card, ATK, HP, Score),
        ( member(Card, Hand),
          troop(Card, ATK, HP, EffectID),
          evaluate(Card, ATK, HP, EffectID, Score, IA_Slot, EnemyField)
        ),
        ScoredMoves),
    % Ordina in base al punteggio (Score) in ordine decrescente
    predsort(compare_tuples, ScoredMoves, SortedMoves),
    SortedMoves = [tuple(Card, ATK, HP, _) | _],
    debug_message("Giocata troop: ", (Card, ATK, HP, IA_Slot)).

% --- Comparatore per predsort/3 ---
% Se Score1 > Score2 allora il termine con Score1 deve comparire PRIMA, per questo restituiamo '<'
compare_tuples(Order, tuple(_,_,_,Score1), tuple(_,_,_,Score2)) :-
    ( Score1 > Score2 -> Order = '<'
    ; Score1 < Score2 -> Order = '>'
    ; Order = '='
    ).

% --- Nuova funzione di valutazione ---
% La valutazione considera il punteggio base (ATK + HP) e, se esiste una carta nemica nell'enemy slot attaccato,
% aggiunge un bonus:
%   - Bonus +3 se ATK IA >= HP nemico
%   - Altrimenti, bonus +2 se HP IA > ATK nemico
% Inoltre, se la carta ha un EFFECT_ID = 1, viene aggiunto un bonus ulteriore di +3.
evaluate(Card, ATK, HP, EffectID, Score, IA_Slot, EnemyField) :-
    BaseScore is ATK + HP,
    (   attacked_enemy_card(IA_Slot, EnemyField, EnemyATK, EnemyHP)
    ->  ( (ATK >= EnemyHP)
         -> Bonus = 3
         ;  (HP > EnemyATK
             -> Bonus = 2
             ;  Bonus = 0)
         )
    ;   Bonus = 0
    ),
    ( EffectID =:= 1 -> EffectBonus = 3 ; EffectBonus = 0 ),
    Score is BaseScore + Bonus + EffectBonus.

% Predicato che determina le statistiche della carta nemica che verrebbe attaccata
attacked_enemy_card(IA_Slot, EnemyField, EnemyATK, EnemyHP) :-
    mirrored_enemy_slot(IA_Slot, EnemySlot),
    member(slot(EnemySlot, EnemyCard), EnemyField),
    troop(EnemyCard, EnemyATK, EnemyHP, _).

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
