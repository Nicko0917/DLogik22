subsetx([], _, []).
subsetx([E|Tail], L, [E|NTail]):-
  subsetx(Tail, _, NTail), length([E|NTail], L).
subsetx([_|Tail], L1, NTail):-
  subsetx(Tail, L1, NTail).