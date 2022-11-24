verify(InputFileName) :- see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof).

valid_proof(Premises, Target, Proof) :-
        (last_row_matches_target(Target,Proof), go_through_list(Proof, Premises, Proof)
        ->  
        write('yes \n');
        write('no \n')).

go_through_list([], _, _).
go_through_list([H|T], Premises, Proof) :- go_through_list(T,Premises, Proof),
    (member(X, H), is_list(X) -> go_through_list(H, Premises, Proof)
    ; pick_rule(H, Premises, Proof)).



/* pick_rule(H, Premises, Proof), go_through_list(T,Premises, Proof). */

pick_rule([Nr|[P,Rule]], Premises, Proof) :- 
(
    Rule=premise -> premise([Nr|[P,Rule]], Premises)
;   Rule=andin(A,B) -> andin(Nr, A, B, Proof, P)
;   Rule=impel(A,B) -> impel(Nr, A, B, Proof, P)
;   Rule=negel(A,B) -> negel(Nr, A, B, Proof)
;   Rule=negint(A,B) -> negint(Nr, A, B, Proof)
;   Rule=assumption -> true
).

negint(CurrentRowNr, A, B, Proof) :-
    B<CurrentRowNr, A<CurrentRowNr-1,
    existsRow(Proof,A,X,assumption), existsRow(Proof,B,_,negel(_,_)),
    existsRow(Proof,CurrentRowNr,neg(X),_).
    /* member([A|[X,assumption]], Proof), member([B|[_,negel(_,_)]], Proof),
    member([CurrentRowNr|[neg(X),_]], Proof) */

negel(CurrentRowNr, A, B, Proof) :-
    A < CurrentRowNr, B < CurrentRowNr,
    existsRow(Proof,A,X,_), existsRow(Proof,B,neg(X),_)
    ; existsRow(Proof,A,neg(X),_), existsRow(Proof,B,X,_).
    /* (member([A|[X,_]], Proof), member([B|[neg(X),_]], Proof)
    ; member([A|[neg(X),_]], Proof), member([B|[X,_]], Proof)) */

impel(CurrentRowNr, A, B, Proof, P) :-
    A < CurrentRowNr, B < CurrentRowNr,
    existsRow(Proof,A,X,_), existsRow(Proof,B,imp(X,P),_).
    /* member(Z, Proof), member([A|[X,_]], Z), member([B|[imp(X,P),_]], Proof) */

existsRow(Proof,A,B,C) :- member([A|[B,C]],Proof), !.
existsRow(Proof,A,B,C) :- member(X,Proof), existsRow(X, A, B, C). 

andin(CurrentRowNr, A, B, Proof, P) :- 
    A < CurrentRowNr, B < CurrentRowNr,
    P=and(X,Y), member([A|[X,_]], Proof), member([B|[Y,_]], Proof).

premise([_|[P,_]], Premises) :- member(P, Premises).

last(X,[X]).
last(X,[_|Z]) :- last(X,Z).

last_row_matches_target(Target, Proof) :-
    last([_|[P,_]], Proof), P=Target.

