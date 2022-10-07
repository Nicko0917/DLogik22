edge(1,2).
edge(1,4).
edge(1,3).
edge(2,3).
edge(2,5).
edge(3,4).
edge(3,5).
edge(4,5).

/* True if there is an edge between the two nodes */
connected(X,Y) :- edge(X,Y) ; edge(Y,X).

path(X, Y, Path) :-
    path(X, Y, [X], Path).

/* True when we've arrived at the target node */
path(X, X, Visited, Visited).
/* True if  there is an edge between the current node and another node
that has not been visited and that has a path to the target node.*/
path(X, Z, Visited, Path):-
    connected(X, Y),
    /* Not member of visited */
    \+ ismember(Y, Visited),
    /* Recursion. Add node to head of list and keep locking for Z */
    path(Y, Z, [Y|Visited], Path).

/* Go through list and check if the the element exists. 
The head of the list will be removed until it's found or the whole list has been  checked.*/
ismember(X, [X|_]) :- !.
ismember(X, [_|Z]) :- ismember(X, Z).