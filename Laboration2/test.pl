existsRow(Proof,A,B,C) :- member([A|[B,C]],Proof), !.
existsRow(Proof,A,B,C) :- member(X,Proof), x(X, A, B, C). 