%  ================================== 2012 ===================================

% 5.
father(george, elisabeth).
father(philip, charles).
father(charles, william).
mother(elisabeth, charles).
mother(diana, william).
mother(diana, henry).

% 5. a)
q5a :- mother(X, Y), mother(X, Z), Y \= Z.

% 5. b)
grandfather(X, Z) :- father(X, Y), father(Y, Z).
q5b :- grandfather(X, Z).

% 5. c)
q5c :- findall(X, mother(M, X), L).