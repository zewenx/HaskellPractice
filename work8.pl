
% workshop8

% Q1
list_of(_, []).
list_of(Ele,[Ele|Tail]) :- list_of(Ele,Tail).

% Q2
all_same(Xs) :- list_of(_,Xs).

% Q3
adjacent(E1, E2, List) :- 
    append(E1,E2,E),
    append([_|E],_,List).

% Q4
adjacent1(E1,E2,[E1,E2|_]).
adjacent1(E1, E2, [_E|List]) :- 
    adjacent1(E1,E2,List).

%Q5 it is wrong with some cases.
before(E1, E2, [E1|List]) :- before(_,E2,List).
before(_,E2,[E2|_]).
before(E1,E2,[_|Tail]):-before(E1,E2,Tail).


%leture 8
ap(A,B,C) :- (
    A = [] -> C = B
    ; A =[U|V], ap(V,B,VB), C=[U|VB]
    ).


calss([],0).
calss([H|T],X) :- 
    int(H),
    X>0,
    Tail is X-H,
    calss(T,Tail).


int(0).
int(1).
int(2).
int(3).
int(4).
int(5).
int(6).
int(7).
int(8).
int(9).