% lecture 7  
% can an instance has a variable
% a term  f(A,B)  a substitution {A -> 2}  instance f(2,B)?

myAppend([],C,C).
myAppend([A|C],B,[A|BC]) :- myAppend(C,B,BC).

myproper_list([]).
myproper_list([_Head|Tail]) :- myproper_list([Tail]).

drop(N, List, Back) :-
    length(X,N),
    myAppend(X,Back,List).

% why is the memebr2 more efficient than member1
member1(Ele,List) :- myAppend(_,[Ele|_],List).

member2(Ele,[Ele|_]).
member2(Ele,[_|Rest]) :- member2(Ele,Rest).

rev1([],[]).
rev1([A|Tail],X) :- 
    rev1(Tail,Tail_),
    myAppend(Tail_,[A],X).


rev2([],[]).
rev2([A|Tail],X) :- 
    myAppend(Tail_,[A],X),
    rev2(Tail,Tail_).
    
rev3(ABC,CBA) :-
    samelength(ABC,CBA),
    rev1(ABC,CBA).

samelength([],[]).
samelength([_|Xa],[_|Ya]) :-
    samelength(Xa,Ya).

take1(N,List,Front) :-
    length(Front,N),
    append(Front,_,List).


take2(N,List,Front) :-
    append(Front,_,List),
    length(Front,N).

%% rev1(X,[1]). will cause infinite loop but reverse(X,[1]) will not. WHY!
%% 6*7 is 7*6. will get a false response. because these two are different term!

