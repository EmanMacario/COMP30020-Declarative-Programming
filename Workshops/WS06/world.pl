:- ensure_loaded(borders).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(rivers).


% Question 4
country(X) :- country(X, _, _, _, _, _, _, _).

% Question 5
larger(X, Y) :- country(X, _, _, _, AreaX, _, _, _),
                country(Y, _, _, _, AreaY, _, _, _),
                AreaX > AreaY.

% Question 6
river_country(River, Country) :- river(River, [X | Xs]),
                                 country(Country),
                                 member(Country, [X | Xs]). 


country_region(Country, Region) :- country(Country, Region, _, _, _, _, _, _).


/* Query:
country_region(C1, R1), country_region(C2, R2), R1 \= R2, 
river_country(River, C1), river_country(River, C2).
*/