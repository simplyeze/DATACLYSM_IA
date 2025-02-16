:- dynamic game_state/2.
:- use_module(library(random)).  % Per l'uso di random_member
:- discontiguous set_game_state/2.
:- ensure_loaded('cardlist').

% --- Predicato per aggiornare lo stato in maniera unica ---
set_game_state(Key, Value) :-
    retractall(game_state(Key, _)),
    assert(game_state(Key, Value)).

% --- Stato iniziale del gioco ---
set_game_state(hand_ia, []).
set_game_state(field_ia, []).
set_game_state(field_enemy, []).
set_game_state(field_setting, setting(102)).
set_game_state(method_played, false).

% --- Predicato principale che decide l'azione migliore ---
decide_action(BestMoves) :-
    game_state(hand_ia, Hand),
    play_setting(Hand, SettingMove),
    play_method(Hand, MethodMove),
    game_state(field_ia, Field),
    game_state(field_enemy, EnemyField),
    (   % Se in mano esiste almeno una carta troop (numero)
        member(Card, Hand),
        number(Card)
    ->  choose_slot(Field, EnemyField, SelectedSlot), % Sceglie slot
        ( SelectedSlot \= none -> % Sceglie migliore carta
             debug_message("Slot scelto: ", SelectedSlot),
             select_best_move(Hand, SelectedSlot, EnemyField, TroopMove) % Dalla mano, tiene conto del campo avversario
        ;   TroopMove = [] % Per memorizzare il risultato
        )
    ;   % Altrimenti, non eseguire mosse troop
        TroopMove = []
    ),
    append([SettingMove, MethodMove, TroopMove], BestMoves), % Combinate le mosse nella lista BestMoves
    debug_message("Mosse scelte: ", BestMoves).

% --- Predicato che sceglie lo slot da usare: preferenza alla difesa ---
choose_slot(Field, EnemyField, SelectedSlot) :-
    find_defensive_slot(Field, EnemyField, DefensiveSlot),
    DefensiveSlot \= none,
    !, % Cut impedisce altre azioni se è stato trovato DefensiveSlot
    SelectedSlot = DefensiveSlot.
choose_slot(Field, EnemyField, SelectedSlot) :-
    find_offensive_slot(Field, EnemyField, OffensiveSlot),
    OffensiveSlot \= none,
    !,
    SelectedSlot = OffensiveSlot.
choose_slot(_, _, none). % Se non è stato trovato slot, diventa none

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
    member(slot(EnemySlot, _, 0), EnemyField), % Ricerca carta avversaria
    mirrored_slot(EnemySlot, Mirror),
    \+ occupied_slot(Field, Mirror), % Verifica che lo slot mirror non sia occupato
    DefensiveSlot = Mirror, !.
find_defensive_slot(_, _, none).

% --- Difesa Offensiva: scelta casuale tra tutti gli slot disponibili (davanti e dietro) ---
find_offensive_slot(Field, EnemyField, AttackSlot) :-
    findall(Candidate,
            (
                member(Candidate, [1,2,3,4,5,6,7,8]),
                \+ occupied_slot(Field, Candidate),
                \+ enemy_slot(EnemyField, Candidate)
            ),
            AvailableSlots),
    ( AvailableSlots \= [] ->
          random_member(AttackSlot, AvailableSlots)
    ;   AttackSlot = none
    ).

% --- Predicati ausiliari ---
occupied_slot(Field, Slot) :-
    member(slot(Slot, _, _), Field).

enemy_slot(EnemyField, Slot) :- % Verifica se lo slot nemico è occupato
    member(slot(Slot, _, _), EnemyField).

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

% --- Selezione della mossa migliore per le carte troop ---
select_best_move(Hand, IA_Slot, EnemyField, TroopMove) :-
    % Cerca solo carte troop (UpdateID = 0)
    findall(tuple(Card, ATK, HP, Score),
        ( member(Card, Hand),
          number(Card),
          troop(Card, ATK, HP, Effects, Subtype, UpdateID),
          UpdateID =:= 0,  % Esclude le carte update
          evaluate(Card, ATK, HP, Effects, Subtype, UpdateID, Score, IA_Slot, EnemyField) % Calcola valutazione carta
        ),
        ScoredMoves),
    ( ScoredMoves = [] -> % Se non ci sono troop valide, lista vuota
         TroopMove = []
    ;
         predsort(compare_tuples, ScoredMoves, SortedMoves), % Ordina lista
         SortedMoves = [tuple(Card, ATK, HP, _) | _], % Seleziona la prima carta
         TroopMove = [(Card, ATK, HP, IA_Slot)],
         debug_message("Giocata troop: ", (Card, ATK, HP, IA_Slot))
    ).

compare_tuples(Order, tuple(_, _, _, Score1), tuple(_, _, _, Score2)) :-
    % Ordinamento decrescente in base al punteggio
    compare(Order, Score2, Score1).

% --- Funzione di valutazione ---
evaluate(_Card, ATK, HP, Effects, _Subtype, _UpdateID, Score, IA_Slot, EnemyField) :-
    BaseScore is ATK + HP, % Calcolo punteggio base
    ( attacked_enemy_card(IA_Slot, EnemyField, EnemyATK, EnemyHP, EnemyEffects) -> % Verifica se la carta è in modalità offensiva o difensiva
          Mode = defense,
          HasEnemy = true
    ;     Mode = attack,
          HasEnemy = false,
          EnemyEffects = []
    ),
    ( Effects \= [] -> % Calcola i bonus se la carta ha effetti
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
    ;   ( HasEnemy = true -> % Calcola punteggio in base al nemico
               ( ATK >= EnemyHP -> TotalBonus = 3
               ; HP > EnemyATK -> TotalBonus = 2
               ; TotalBonus = 0 )
         ;  TotalBonus = 0 )
    ),
    Score is BaseScore + TotalBonus.

attacked_enemy_card(IA_Slot, EnemyField, EnemyATK, EnemyHP, EnemyEffects) :- % Controlla se la carta AI ha davanti una carta dell'avversario
    mirrored_enemy_slot(IA_Slot, EnemySlot),
    member(slot(EnemySlot, EnemyCard, 0), EnemyField), % Recupera le statistiche della carta avversaria
    troop(EnemyCard, EnemyATK, EnemyHP, EnemyEffects, _, _).

% --- Esecuzione e stampa delle mosse ---
esegui(Azioni) :-
    decide_action(Azioni),
    forall(member(Azione, Azioni), stampa_azione(Azione)). % Itera su tutti gli elementi della lista Azioni

stampa_azione(("No Move", 0, 0, 0)) :-
    format('Gioca(No Move, 0, 0, 0)~n').
stampa_azione((Card, "PlaySetting")) :-
    format('Gioca il nuovo Setting: ~w~n', [Card]).
stampa_azione((Card, "PlayMethod")) :-
    format('Gioca il nuovo Method: ~w~n', [Card]).
stampa_azione((Card, _, _, Slot)) :-
    format('Gioca(~w, ~w)~n', [Card, Slot]).
stampa_azione((Card, "Scarta")) :-
    format('Scarta la carta: ~w~n', [Card]).

debug_message(Label, Data) :-
    format("DEBUG: ~w ~w~n", [Label, Data]).

esegui_update :-
    game_state(hand_ia, Hand),
    game_state(field_ia, Field),
    findall((UpdateCard, Slot),
        ( member(UpdateCard, Hand),
          number(UpdateCard),
          % Otteniamo anche la lista degli effetti della carta update
          troop(UpdateCard, _, _, UpdateEffects, _, UUpdateID),
          UUpdateID =\= 0,  % Verifica se in mano ci sono carte update
          % Seleziona slot legali: oppure lo slot ha Flag = 0, oppure la carta ha l'effetto 14
          member(slot(Slot, FieldCard, Flag), Field),
          ( Flag =:= 0 ; member(14, UpdateEffects) ),
          troop(FieldCard, _, _, _, FieldSubtype, FieldUpdateID),
          FieldUpdateID =:= 0,  % Controlla se nel campo ci sono truppe non aggiornate
          UUpdateID =:= FieldSubtype  % Corrispondenza tra UpdateID della carta e Subtype della truppa
        ),
        UpdatePairs),
    ( UpdatePairs = [] ->
         format('Nessun Update giocabile~n')
    ;
         random_member((ChosenUpdate, ChosenSlot), UpdatePairs),
         Move = (ChosenUpdate, 0, 0, ChosenSlot),
         stampa_azione(Move)
    ),
    true.

% --- Predicato per scartare carte in eccesso a fine turno ---
scarta_fine_turno :-
    game_state(hand_ia, Hand),
    length(Hand, NumCards),
    (   NumCards > 7 ->
        Diff is NumCards - 7,
        % Mischia la mano e separa le carte da scartare (Diff) da quelle da tenere
        random_permutation(Hand, ShuffledHand),
        length(Discarded, Diff),
        append(Discarded, NewHand, ShuffledHand),
        set_game_state(hand_ia, NewHand),
        % Per ogni carta da scartare, stampa il messaggio richiesto
        forall(member(Card, Discarded),
               format('Scarta la carta: ~w~n', [Card]))
    ;   % Se non ci sono carte da scartare, stampa "Nessuno scarto!"
        format('Nessuno scarto!~n')
    ).
