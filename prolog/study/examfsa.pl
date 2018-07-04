startState(1).
finalState(3).

transition(1, a, 1).
transition(1, b, 2).
transition(2, b, 3).
transition(3, a, 3).
transition(1, c, 3).

accept(Sentence) :- startState(State), pathToFinal(State, Sentence).

pathToFinal(State, []) :- finalState(State).
pathToFinal(State0, [Event | SentenceRest]) :-
  transition(State0, Event, State1),
  pathToFinal(State1, SentenceRest).