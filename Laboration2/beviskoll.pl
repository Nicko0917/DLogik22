verify(InputFileName) :- see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof).

valid_proof(Premises, Target, Proof) :-
        last_row_matches_target(Target,Proof), go_through_list(Proof, Premises, Proof).

go_through_list([], _, _).
go_through_list([H|T], Premises, Proof) :- go_through_list(T,Premises, Proof),
    (member(X, H), is_list(X) -> go_through_list(H, Premises, Proof)
    ; pick_rule(H, Premises, Proof)).

pick_rule([Nr|[P,Rule]], Premises, Proof) :- 
(
    Rule=premise -> premise([Nr|[P,Rule]], Premises)
;   Rule=andint(A,B) -> andint(Nr, A, B, Proof, P), closedBoxCheck(Proof, Nr, A), closedBoxCheck(Proof, Nr, B)
;   Rule=impel(A,B) -> impel(Nr, A, B, Proof, P), closedBoxCheck(Proof, Nr, A), closedBoxCheck(Proof, Nr, B)
;   Rule=negel(A,B) -> negel(Nr, A, B, Proof), closedBoxCheck(Proof, Nr, A), closedBoxCheck(Proof, Nr, B)
;   Rule=negint(A,B) -> negint(Nr, A, B, Proof), boxConclusion(Proof, Nr, A, B)
;   Rule=assumption -> assumption(Nr, Proof)
;   Rule=copy(A) -> copy(Nr, A, Proof, P), closedBoxCheck(Proof, Nr, A)
;   Rule=impint(A,B) -> impint(Nr, A, B, Proof, P), boxConclusion(Proof, Nr, A, B)
;   Rule=orel(A,B,C,D,E) -> orel(Proof, P, A,B,C,D,E, Nr), closedBoxCheck(Proof, Nr, A), 
        boxConclusion(Proof, Nr, B, C), boxConclusion(Proof, Nr, D, E)
;   Rule=pbc(A,B) -> pbc(Proof, P, A, B, Nr), boxConclusion(Proof, Nr, A, B)
;   Rule=andel1(A) -> andel1(Proof, P, A, Nr), closedBoxCheck(Proof, Nr, A)
;   Rule=andel2(A) -> andel2(Proof, P, A, Nr), closedBoxCheck(Proof, Nr, A)
;   Rule=contel(A) -> contel(Proof, A, Nr), closedBoxCheck(Proof, Nr, A)
;   Rule=negnegel(A) -> negnegel(Proof, P, A, Nr), closedBoxCheck(Proof, Nr, A)
;   Rule=negnegint(A) -> negnegint(Proof, P, A, Nr), closedBoxCheck(Proof, Nr, A)
;   Rule=lem -> lem(Proof, Nr)
;   Rule=mt(A,B) -> mt(Proof, P, A, B, Nr), closedBoxCheck(Proof, Nr, A), closedBoxCheck(Proof, Nr, B)
).

assumption(CurrentRowNr, Proof) :-
    member([[CurrentRowNr|[_,assumption]]|_],Proof).
assumption(CurrentRowNr, Proof) :-
    member(X,Proof), assumption(CurrentRowNr, X).

mt(Proof, P, A, B, CurrentRowNr) :-
    A<CurrentRowNr, B<CurrentRowNr,
    P=neg(X), existsRow(Proof, A, imp(X,Y), _), 
    existsRow(Proof, B, neg(Y), _).
    

lem(Proof, CurrentRowNr) :- 
    existsRow(Proof, CurrentRowNr, or(X,neg(X)), _).

negnegint(Proof, P, A, CurrentRowNr) :-   
    A<CurrentRowNr,
    existsRow(Proof, A, X, _), P=neg(neg(X)).

negnegel(Proof, P, A, CurrentRowNr) :-
    A<CurrentRowNr,
    existsRow(Proof, A, neg(neg(P)), _).

contel(Proof, A, CurrentRowNr) :-
    A<CurrentRowNr,
    existsRow(Proof, A, cont, _).

andel2(Proof, P, A, CurrentRowNr) :-
    A<CurrentRowNr,
    existsRow(Proof, A, and(_,P), _).

andel1(Proof, P, A, CurrentRowNr) :-
    A<CurrentRowNr,
    existsRow(Proof, A, and(P,_), _).

pbc(Proof, P, A, B, CurrentRowNr) :-
    A<CurrentRowNr, B<CurrentRowNr,
    existsRow(Proof, A, neg(P), assumption), existsRow(Proof, B, cont, _).

orel(Proof, P, A,B,C,D,E, CurrentRowNr) :-  
    A<CurrentRowNr, C<CurrentRowNr, E<CurrentRowNr,     
    existsRow(Proof, A, or(X,Y), _), existsRow(Proof, B, X, assumption), 
    existsRow(Proof, D, Y, assumption), existsRow(Proof, C, P, _),
    existsRow(Proof, E, P, _).

impint(CurrentRowNr, A, B, Proof, P) :-
    A<CurrentRowNr, B<CurrentRowNr, 
    existsRow(Proof, A, X, assumption), existsRow(Proof, B, Y, _),
    P=imp(X,Y).

boxConclusion(X, CurrentRowNr, A, B) :-
    member([CurrentRowNr|_], X), member(Z, X), 
    member([A|_], Z), member([B|_], Z).
boxConclusion(X, CurrentRowNr, A, B) :-
    member(Z, X), boxConclusion(Z, CurrentRowNr, A, B).

copy(CurrentRowNr, A, Proof, P) :-
    A<CurrentRowNr, existsRow(Proof, A, P, _).

negint(CurrentRowNr, A, B, Proof) :-
    B<CurrentRowNr, A<CurrentRowNr-1,
    existsRow(Proof,A,X,assumption), existsRow(Proof,B,_,negel(_,_)),
    existsRow(Proof,CurrentRowNr,neg(X),_).

negel(CurrentRowNr, A, B, Proof) :-
    A < CurrentRowNr, B < CurrentRowNr,
    existsRow(Proof,A,X,_), existsRow(Proof,B,neg(X),_)
    ; existsRow(Proof,A,neg(X),_), existsRow(Proof,B,X,_).
   

impel(CurrentRowNr, A, B, Proof, P) :-
    A < CurrentRowNr, B < CurrentRowNr,
    existsRow(Proof,A,X,_), existsRow(Proof,B,imp(X,P),_).
  
existsRow(Proof,A,B,C) :- member([A|[B,C]],Proof), !.
existsRow(Proof,A,B,C) :- member(X,Proof), existsRow(X, A, B, C).

closedBoxCheck(Proof, CurrentRowNr, RowNr) :- 
    member([RowNr|[_,_]],Proof), existsRow(Proof, CurrentRowNr, _, _), !.
closedBoxCheck(Proof, CurrentRowNr, RowNr) :-    
    member(X, Proof), closedBoxCheck(X, CurrentRowNr, RowNr).



andint(CurrentRowNr, A, B, Proof, P) :- 
    A < CurrentRowNr, B < CurrentRowNr,
    P=and(X,Y), existsRow(Proof, A, X, _), existsRow(Proof, B, Y, _).

premise([_|[P,_]], Premises) :- member(P, Premises).

last(X,[X]).
last(X,[_|Z]) :- last(X,Z).

last_row_matches_target(Target, Proof) :-
    last([_|[P,_]], Proof), P=Target.

