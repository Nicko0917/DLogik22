edge(1,2).
edge(1,4).
edge(1,3).
edge(2,3).
edge(2,5).
edge(3,4).
edge(3,5).
edge(4,5).

connected(X,Y) :- edge(X,Y) ; edge(Y,X).

path(X, Y, Path) :-
    path(X, Y, [X], Path).

path(X, X, Visited, Visited).
path(X, Z, Visited, Path):-
    connected(X, Y),
    \+ ismember(Y, Visited),
    path(Y, Z, [Y|Visited], Path).

ismember(X, [X|_]) :- !.
ismember(X, [_|Z]) :- ismember(X, Z).