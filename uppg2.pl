remove_duplicates(L1, L2) :-
    remove_duplicates(L1, L2, []).
remove_duplicates([], [], _).

/* If head has been seen */
remove_duplicates([H|T], R, Seen) :-
    member(H, Seen), !, (R = S, Seen1 = Seen), remove_duplicates(T, S, Seen1).
/* If head has not been seen*/
remove_duplicates([H|T], R, Seen) :- (R = [H|S], Seen1 = [H|Seen]), remove_duplicates(T, S, Seen1).

/* Detta är en funktion eftersom en input ger ett utvärde */

