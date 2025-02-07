:- dynamic game_state/2.
:- use_module(library(random)).  % Per l'uso di random_member/2

% --- Predicato per aggiornare lo stato in maniera unica ---
set_game_state(Key, Value) :-
    retractall(game_state(Key, _)),
    assert(game_state(Key, Value)).

% --- Carte nel deck (troop/4: Id, ATK, HP, Effects) ---
troop(001, 1, 1, [1]).           % Nessun effetto
troop(002, 1, 1, [1]).           % Ad es. una carta con effetti Start (ID 4) e Collapse (ID 5)
troop(003, 1, 1, [2]).           % Reach (ID 2)
troop(004, 0, 6, []).            % Nessun effetto
troop(005, 1, 1, [3]).           % Override (ID 3)
troop(006, 1, 1, [3]).
troop(007, 1, 1, [3]).
troop(008, 3, 4, [3,2]).
troop(009, 1, 1, [3]).
troop(010, 1, 1, [3]).

% --- Stato iniziale del gioco ---
set_game_state(hand_ia, []).             % Inizialmente la mano è vuota
set_game_state(field_ia, []).             % Campo IA vuoto
set_game_state(field_enemy, []).          % Campo nemico vuoto
set_game_state(field_setting, setting(102)).
set_game_state(method_played, false).

% --- Predicato principale che decide l'azione migliore ---
decide_action(BestMoves) :-
    game_state(hand_ia, Hand),
    play_setting(Hand, SettingMove),
    play_method(Hand, MethodMove),
    game_state(field_ia, Field),
    game_state(field_enemy, EnemyField),
    (   % Se nella mano esiste almeno una carta troop (numero)
        member(Card, Hand),
        number(Card)
    ->  choose_slot(Field, EnemyField, SelectedSlot),
        ( SelectedSlot \= none ->
             debug_message("Slot scelto: ", SelectedSlot),
             select_best_move(Hand, SelectedSlot, EnemyField, TroopMove)
        ;   TroopMove = []
        )
    ;   % Altrimenti, se non ci sono carte troop, non giocare alcuna mossa troop
        TroopMove = []
    ),
    append([SettingMove, MethodMove, TroopMove], BestMoves),
    debug_message("Mosse scelte: ", BestMoves).

% --- Predicato che sceglie lo slot da usare: preferenza alla difesa ---
choose_slot(Field, EnemyField, SelectedSlot) :-
    find_defensive_slot(Field, EnemyField, DefensiveSlot),
    DefensiveSlot \= none,
    !,
    SelectedSlot = DefensiveSlot.
choose_slot(Field, EnemyField, SelectedSlot) :-
    find_offensive_slot(Field, EnemyField, OffensiveSlot),
    OffensiveSlot \= none,
    !,
    SelectedSlot = OffensiveSlot.
choose_slot(_, _, none).

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

% --- Difesa basata sul mirror della slot nemica ---
find_defensive_slot(Field, EnemyField, DefensiveSlot) :-
    member(slot(EnemySlot, _), EnemyField),
    mirrored_slot(EnemySlot, Mirror),
    \+ occupied_slot(Field, Mirror),
    DefensiveSlot = Mirror, !.
find_defensive_slot(_, _, none).

% --- Difesa Offensiva (scelta pseudocasuale degli slot disponibili) ---
find_offensive_slot(Field, EnemyField, AttackSlot) :-
    findall(Candidate, (
        member(Candidate, [1,2,3,4]),
        \+ occupied_slot(Field, Candidate),
        \+ enemy_slot(EnemyField, Candidate)
    ), OffensiveFrontSlots),
    (   OffensiveFrontSlots \= [] ->
            random_member(AttackSlot, OffensiveFrontSlots)
    ;   findall(Candidate, (
            member(Candidate, [5,6,7,8]),
            \+ occupied_slot(Field, Candidate)
    ), OffensiveBackSlots),
        (   OffensiveBackSlots \= [] ->
                random_member(AttackSlot, OffensiveBackSlots)
        ;   AttackSlot = none
        )
    ).

% --- Predicati ausiliari ---
occupied_slot(Field, Slot) :-
    member(slot(Slot, _), Field).

enemy_slot(EnemyField, Slot) :-
    member(slot(Slot, _), EnemyField).

% --- Mappatura per i mirrored_slot ---
mirrored_slot(4, 1).
mirrored_slot(8, 1).
mirrored_slot(3, 2).
mirrored_slot(7, 2).
mirrored_slot(2, 3).
mirrored_slot(6, 3).
mirrored_slot(1, 4).
mirrored_slot(5, 4).

mirrored_enemy_slot(IA_Slot, EnemySlot) :-
    mirrored_slot(EnemySlot, IA_Slot).

select_best_move(Hand, IA_Slot, EnemyField, TroopMove) :-
    % Cerca solo carte troop (ovvero numeri, non strutture method/1)
    findall(tuple(Card, ATK, HP, Score),
        ( member(Card, Hand),
          number(Card),  % Assicura che Card sia un numero, quindi una troop
          troop(Card, ATK, HP, Effects),
          evaluate(Card, ATK, HP, Effects, Score, IA_Slot, EnemyField)
        ),
        ScoredMoves),
    ( ScoredMoves = [] ->
         TroopMove = []  % Non c'è nessuna carta troop, quindi non eseguire mosse troop
    ; 
         predsort(compare_tuples, ScoredMoves, SortedMoves),
         SortedMoves = [tuple(Card, ATK, HP, _) | _],
         TroopMove = [(Card, ATK, HP, IA_Slot)],
         debug_message("Giocata troop: ", (Card, ATK, HP, IA_Slot))
    ).

% --- Funzione di valutazione ---
evaluate(_Card, ATK, HP, Effects, Score, IA_Slot, EnemyField) :-
    BaseScore is ATK + HP,
    ( attacked_enemy_card(IA_Slot, EnemyField, EnemyATK, EnemyHP, EnemyEffects) ->
          Mode = defense,
          HasEnemy = true
    ;     Mode = attack,
          HasEnemy = false,
          EnemyEffects = []
    ),
    ( Effects \= [] ->
         ( HasEnemy = true ->
              ( member(1, Effects) -> ( member(1, EnemyEffects) -> BonusFloat = 2 ; BonusFloat = -2 ) ; BonusFloat = 0 ),
              ( member(2, Effects) -> ( member(1, EnemyEffects) -> BonusReach = 4 ; BonusReach = 1 ) ; BonusReach = 0 ),
              ( member(3, Effects) -> BonusOverride = 4 ; BonusOverride = 0 ),
              ( member(4, Effects) -> BonusStart = 2 ; BonusStart = 0 ),
              ( member(5, Effects) -> ( Mode = defense -> BonusCollapse = 3 ; BonusCollapse = 1 ) ; BonusCollapse = 0 ),
              ( member(6, Effects) -> ( Mode = attack   -> BonusOnHit = 3  ; BonusOnHit = 1 ) ; BonusOnHit = 0 ),
              ( member(7, Effects) -> BonusOnGuard = 2 ; BonusOnGuard = 0 )
         ;   ( member(1, Effects) -> BonusFloat = 3 ; BonusFloat = 0 ),
              ( member(2, Effects) -> BonusReach = 1 ; BonusReach = 0 ),
              ( member(3, Effects) -> BonusOverride = 4 ; BonusOverride = 0 ),
              ( member(4, Effects) -> BonusStart = 2 ; BonusStart = 0 ),
              ( member(5, Effects) -> BonusCollapse = 3 ; BonusCollapse = 0 ),
              ( member(6, Effects) -> BonusOnHit = 1 ; BonusOnHit = 0 ),
              ( member(7, Effects) -> BonusOnGuard = 2 ; BonusOnGuard = 0 )
         ),
         TotalBonus is BonusFloat + BonusReach + BonusOverride +
                        BonusStart + BonusCollapse + BonusOnHit + BonusOnGuard
    ;   ( HasEnemy = true ->
               ( ATK >= EnemyHP -> TotalBonus = 3
               ; HP > EnemyATK -> TotalBonus = 2
               ; TotalBonus = 0 )
         ;  TotalBonus = 0 )
    ),
    Score is BaseScore + TotalBonus.

attacked_enemy_card(IA_Slot, EnemyField, EnemyATK, EnemyHP, EnemyEffects) :-
    mirrored_enemy_slot(IA_Slot, EnemySlot),
    member(slot(EnemySlot, EnemyCard), EnemyField),
    troop(EnemyCard, EnemyATK, EnemyHP, EnemyEffects).

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

debug_message(Label, Data) :-
    format("DEBUG: ~w ~w~n", [Label, Data]).
