%  ================================== 2015 ===================================

% 3.
pre([], _).
pre([X|Y], [X|Z]) :- pre(Y, Z).

% 4.
likes(jim, music).
likes(jim, fishing).
likes(jim, reading).
likes(liz, reading).
likes(liz, dancing).
likes(ann, swimming).
likes(ann, dancing).

% 4. b)
merge([], [], _).
merge([], [X|XS], [X|XS]).
merge([X|XS], [], [X|XS]).
merge([X|Xr], [Y|Yr], [X, Y|Z]) :- X == Y, merge(Xr, Yr, Z).
merge([X|Xr], [Y|Yr], [X|Z]) :- X < Y, merge(Xr, [Y|Yr], Z).
merge([X|Xr], [Y|Yr], [Y|Z]) :- Y < X, merge([X|Xr], Yr, Z).