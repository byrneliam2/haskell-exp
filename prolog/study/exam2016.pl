%  ================================== 2016 ===================================

plays(bale, batman).
plays(affleck, batman).
plays(reeve, superman).
plays(downey, ironman).
plays(downey, sherlock).
plays(cumberbatch, sherlock).

% 1. a)
bale(Y) :- plays(bale, X), plays(Y, X).

% 1. b)
pairs(X, Y) :- plays(X, Z), plays(Y, Z), not(X == Y).

% 1. d)
get(A, C) :- findall(X, plays(A, X), C).