%  ================================== 2016 ===================================

% 4.
plays(bale, batman).
plays(affleck, batman).
plays(reeve, superman).
plays(downey, ironman).
plays(downey, sherlock).
plays(cumberbatch, sherlock).

% 4. a)
bale(Y) :- plays(bale, X), plays(Y, X).

% 4. b)
pairs(X, Y) :- plays(X, Z), plays(Y, Z), not(X == Y).

% 4. d)
get(A, C) :- findall(X, plays(A, X), C).

% 6.
controls(government, fbi).
controls(fbi, police).
controls(government, cia).
controls(police, people).
controls(sarah, tom).
agency(X) :- controls(government, X). % agency must be controlled by govt

% 6. a)
force(X) :- agency(Y), controls(Y, X).

% 6. b)
hasControlOver(X, Y) :- controls(X, Y) ; controls(X, Z), controls(Z, Y).

% 6. c)
limitedControl(X) :- not(agency(X)), not(X == government).