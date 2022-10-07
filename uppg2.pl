/* Detta är en funktion eftersom en input ger endast ett utvärde */

remove_duplicates(L1, L2) :-
    remove_duplicates(L1, L2, []).

remove_duplicates([], [], _).

/* If head has been seen */
remove_duplicates([H|T], R, Seen) :-
    memberx(H, Seen), !, remove_duplicates(T, R, Seen).
/* If head has not been seen
Set H to head of R which is a list with T as tail with the duplicates removed  */
remove_duplicates([H|T], R, Seen) :- (R = [H|S]), remove_duplicates(T, S, [H|Seen]).

memberx(X, [X|_]) :- !.
memberx(X, [_|Z]) :- memberx(X, Z).

