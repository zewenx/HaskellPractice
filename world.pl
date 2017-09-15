:- ensure_loaded(borders).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(rivers).

country(C) :- country(C,_,_,_,_,_,_,_).

larger(C1,C2) :- 
    country(C1,_,_,_,A1,_,_,_),
    country(C2,_,_,_,A2,_,_,_),
    A1>A2.

river_country(River, Country) :-
    river(River,Countries),
    member(Country,Countries),
    country(Country).

country_region(Country, Region) :-
    country(Country,Region,_,_,_,_,_,_).