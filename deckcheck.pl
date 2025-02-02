% Implementazione della libreria CSV
:- use_module(library(csv)).

% Lettura file CSV, 
deck(File) :-
	% Cambiare arity per valori necessari
    csv_read_file(File, Rows, [functor(row), arity(5)]),
    % Selezione carte "in"
    findall(Card,
            ( member(Row, Rows),
			% Aggiungere valori necessari in ln12, ln15, ln16
              Row = row(InField, Name, Atk, Hp, _Extra),
              atom_string(InField, InFieldStr),
              string_lower(InFieldStr, LowerInField),
              sub_string(LowerInField, _, _, _, "in"),
              Card = card(Name, Atk, Hp)
            ),
            CardList),
			maplist(print_card, CardList).

% Debug carte			
print_card(Card):-
	format("~w~n", [Card]).