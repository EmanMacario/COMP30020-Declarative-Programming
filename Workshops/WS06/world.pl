:- ensure_loaded(borders).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(rivers).


% Question 1 - Write a query to find what borders Australia.
% ?- borders(australia, X).


% Question 2 - Query to find what shares a border with both France and Spain
% ?- borders(X, france), borders(X, spain).


% Question 3 - Give a query to find what countries share a border with both
% France and Spain. Use the country/2 query.
% ?- borders(X, france), borders(X, spain), country(X).


% Question 4
country(X) :- country(X, _, _, _, _, _, _, _).


% Question 5
larger(C1, C2) :- country(C1, _, _, _, A1, _, _, _),
                  country(C2, _, _, _, A2, _, _, _),
                  A1 > A2.

% Question 6
river_country(River, Country) :- river(River, CountryList),
                                 country(Country),
                                 member(Country, CountryList). 


country_region(Country, Region) :- country(Country, Region, _, _, _, _, _, _).


% Give a query to find a river that flows between countries in different
% regions.
% ?- country_region(C1, R1), 
%    country_region(C2, R2), 
%    R1 \= R2, 
%    river_country(River, C1), 
%    river_country(River, C2).