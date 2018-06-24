%  ================================== 2013 ===================================

% 5.
hasRead(tim, robinHood).
hasRead(tim, mobyDick).
hasRead(hugo, bible).
hasRead(jack, phoneBook).
hasRead(jack, bible).
hasRead(jill, phoneBook).

% 5. a)
q5a :- hasRead(jack, X), hasRead(Y, X), Y \= jack.

% 5. b)
q5b :- hasRead(X, Y), hasRead(Z, Y), X \= Z.

% 5. d)
q5d :- findall(X, hasRead(P, X), Y).

% 7.
friendOf(caesar, lucullus).
friendOf(caesar, gaius).
friendOf(merula, sulla).
friendOf(lucullus, lucius).
friendOf(gaius, marius).

% 7. a)
powerful(X) :- friendOf(caesar, X).

% 7. b)
influential(X) :- friendOf(Y, X), powerful(Y).

% 7. c)
acquaintedWith(X, Y) :- friendOf(X, Y).
acquaintedWith(X, Y) :- friendOf(X, Z), acquaintedWith(Z, Y).

% 7. d)
unimportant(X) :- not(influential(X)).