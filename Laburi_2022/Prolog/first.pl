% parent(andrei, bogdan).
% parent(andrei, bianca).
% parent(bogdan, cristi).

% grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

% CA, CC - 2019, va
% ex 8

% p(R, S) :- member(X, R), findall(Y, (member(Y, R), Y \= X), T), !, q(X, T, S).
% q(X, A, [X|A]).
% q(X, [A|B], [A|C]) :- q(X, B, C).

% interogarea: p([1, 2, 3, 4], S).

nod(a).
nod(b).
nod(c).
arc(a, b).
arc(b, a).
arc(b, c).
arc(c, a).
arc(c, b).

succ(X) :- forall(nod(X), (arc(X, Y), Y \= X, arc(X, Z), Z \= X, Z \= Y)).


