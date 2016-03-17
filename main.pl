append([],L,L). % empty list and L is just L
append([A|L],L1,[A|L2]) :- % append
  append(L,L1,L2).



% i want to append one, two...do the second first


% think of it as: when you execute a query, you are trying to prove
%  a rule. and to prove, you have to dive into the rhs
setMember(X,[X|_]).
setMember(X,[_|Y]) :-
    setMember(X,Y).

%%% Question 1

xreverse(List, Reverse) :- % public api
  xreverse(List, [], Reverse).

xreverse([], Result, Result). % base case
xreverse([Head|Tail], Partial, Reverse) :- % worker
  xreverse(Tail, [Head|Partial], Reverse).

%%% Question 2

notMember(_, []). % last element not the same
notMember(X, [Y|Rest]) :-
  X \== Y,
  notMember(X, Rest).

delete([Head|Tail],Head,TailNew) :-
  delete(Tail,Head,TailNew). % delete all Head from Tail

delete([],_,[]). % base case
delete([Head|Tail],E,[Head|TailNew]) :-
  Head \== E,
  delete(Tail,E,TailNew).

xunique([],[]). % base case
xunique([Head|Tail], [Head|TailUnique]) :-
  delete(Tail, Head, TailNew), % delete all element Head from Tail to make TailNew
  xunique(TailNew, TailUnique). % recursive step

%%% Question 3

xunion(One, Two, Result) :-
  append(One, Two, AppendResult),
  xunique(AppendResult, Result).

%%% Question 4

removeLast([Last], [], Last). % list has one element, result is empty
removeLast([H|T], [H|WithoutLast], L) :- % list of two or more,
  removeLast(T, WithoutLast, L).

%%% Question 5

% 5.1

node(a).
node(b).
node(c).
node(d).
node(e).

edge(a,b).
edge(b,c).
edge(c,a).
edge(d,a).
edge(a,e).

subset([], _).
subset([X|Xs], Set) :-
  append(_, [X|Set1], Set),
  subset(Xs, Set1).

clique(L) :-
  findall(X, node(X),Nodes), % nodes is a list of all the nodes
  subset(L, Nodes), % L must be a subset of all the nodes to be valid
  allConnected(L). % each in L must be connected to all others in L

allConnected([]).
allConnected([A|L]) :-
  connect(A, L),
  allConnected(L).

% check if A is connected for every node in L
connect(_, []).
connect(A, [H|L]) :-
  (edge(A, H); edge(H, A)), % edge from either side
  connect(A, L). % and recure

% 5.2

allCliques([], _).
allCliques([First|Rest], [First|Result]) :- % is a clique
  clique(First),
  allCliques(Rest, Result).
allCliques([_|Rest], Result) :- % is not a clique
  allCliques(Rest, Result).

allMaximalCliques([], _, []).
allMaximalCliques([First|Rest], All, [First|Result]) :- % add, it's a maximal clique
  noSuperSets(First, All),
  allMaximalCliques(Rest, All, Result).
allMaximalCliques([_|Rest], All, Result) :- % don't add, not a maximal clique
  allMaximalCliques(Rest, All, Result).

noSuperSets(E, []).
noSuperSets(E, [First|Rest]) :-
  (E == First ; notSubset(E, First)),
  noSuperSets(E, Rest).

notSubset([], S) :- false. % empty is a subset of anything
notSubset([First|Rest], S) :-
  notMember(First, S).
notSubset([First|Rest], S) :-
  notSubset(Rest, S).

ofLengthN([], _, []).
ofLengthN([First|Rest], N, [First|Result]) :- % add to list
  length(First, N),
  ofLengthN(Rest, N, Result).
ofLengthN([First|Rest], N, Result) :- % don't add to list
  ofLengthN(Rest, N, Result).

allSubsets([], []).
allSubsets([E|Tail], [E|NTail]):-
  allSubsets(Tail, NTail).
allSubsets([_|Tail], NTail):-
  allSubsets(Tail, NTail).

maxclique(N, Cliques) :-
  findall(X, node(X), Nodes), % all the nodes
  findall(Y, allSubsets(Nodes, Y), AllCandidates), % # find all perms of the nodes - 32 is all the perms
  allCliques(AllCandidates, AllCliques), % # find all cliques -- 12 is length of AllCliques
  allMaximalCliques(AllCliques, AllCliques, AllMaximalCliques), % working, i think
  ofLengthN(AllMaximalCliques, N, Cliques). % find all lists of length N
