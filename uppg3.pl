/* First part will remove the head of the first list
until the heads of the lists are the same. It will then go to sequence */
partstring([], _, []).
partstring([_|Tail], L, List) :-
    partstring(Tail, L, List).
partstring([H|Tail1], L, [H|Tail2]) :-
    sequence(Tail1, Tail2), length([H|Tail2], L).

/* Will return false if heads are not equal.
Removes heads of the lists until the sub-sequence is an empty list.  */
sequence(_, []).
sequence([H|Tail1], [H|Tail2]) :-
    sequence(Tail1, Tail2).