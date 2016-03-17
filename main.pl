%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%

append([],L,L). % empty list and L is just L
append([A|L],L1,[A|L2]) :- % append
  append(L,L1,L2).

setMember(X,[X|_]).
setMember(X,[_|Y]) :-
    setMember(X,Y).

%%%%%%%%%%%%%%%%%% Question 1 %%%%%%%%%%%%%%%%%%%%%%

% public api
xreverse(List, Reverse) :-
  xreverse(List, [], Reverse).

xreverse([], Result, Result). % base case
xreverse([Head|Tail], Partial, Reverse) :- % worker
  xreverse(Tail, [Head|Partial], Reverse).

%%%%%%%%%%%%%%%%%% Question 2 %%%%%%%%%%%%%%%%%%%%%%

xunique([],[]). % base case
xunique([Head|Tail], [Head|TailUnique]) :-
  delete(Tail, Head, TailNew), % delete all Head from Tail, return TailNew
  xunique(TailNew, TailUnique).

% case when head is the target looking to delete
delete([Head|Tail],Head,TailNew) :-
  delete(Tail,Head,TailNew).

% case base
delete([],_,[]).

% case when head is not the same as targt looking to delete, keep in TailNew
delete([Head|Tail],E,[Head|TailNew]) :-
  Head \== E,
  delete(Tail,E,TailNew).

%%%%%%%%%%%%%%%%%% Question 3 %%%%%%%%%%%%%%%%%%%%%%

xunion(One, Two, Result) :-
  append(One, Two, AppendResult),
  xunique(AppendResult, Result).

%%%%%%%%%%%%%%%%%% Question 4 %%%%%%%%%%%%%%%%%%%%%%

% list has one element, result is empty
removeLast([Last], [], Last).

% list of two or more
removeLast([H|T], [H|WithoutLast], L) :-
  removeLast(T, WithoutLast, L).

%%%%%%%%%%%%%%%%%% Question 5 %%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%% 5.1 %%%%%%%%%%%%%%%%%%%%%%

% TODO: remove these
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

% checks if param one is a subset of param two
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
  connect(A, L), % A connected to all others in L
  allConnected(L).

% check if A is connected for every node in L
connect(_, []).
connect(A, [H|L]) :-
  (edge(A, H); edge(H, A)), % edge from either side
  connect(A, L). % and recure

%%%%%%%%%%%%%%%%%% 5.2 %%%%%%%%%%%%%%%%%%%%%%

% add to result if is a clique
allCliques([], _).
allCliques([First|Rest], [First|Result]) :- % is a clique
  clique(First),
  allCliques(Rest, Result).
allCliques([_|Rest], Result) :- % is not a clique
  allCliques(Rest, Result).

% add to result if has no supersets
allMaximalCliques([], _, []).
allMaximalCliques([First|Rest], All, [First|Result]) :- % add, it's a maximal clique
  noSuperSets(First, All),
  allMaximalCliques(Rest, All, Result).
allMaximalCliques([_|Rest], All, Result) :- % don't add, not a maximal clique
  allMaximalCliques(Rest, All, Result).

% check E against all in list param two and make sure it has no supersets
noSuperSets(_, []).
noSuperSets(E, [First|Rest]) :-
  (E == First ; notSubset(E, First)),
  noSuperSets(E, Rest).

% check that X not container in list param two
notMember(_, []).
notMember(X, [Y|Rest]) :-
  X \== Y,
  notMember(X, Rest).

% check that param one is not a subset of param two
notSubset([], _) :- false. % empty is a subset of anything
notSubset([First|_], S) :-
  notMember(First, S).
notSubset([_|Rest], S) :-
  notSubset(Rest, S).

% filter list of lists by having length exactly N
ofLengthN([], _, []).
ofLengthN([First|Rest], N, [First|Result]) :- % add to list
  length(First, N),
  ofLengthN(Rest, N, Result).
ofLengthN([_|Rest], N, Result) :- % don't add to list
  ofLengthN(Rest, N, Result).

% will generate all the subsets of a list
allSubsets([], []).
allSubsets([E|Tail], [E|NTail]):-
  allSubsets(Tail, NTail).
allSubsets([_|Tail], NTail):-
  allSubsets(Tail, NTail).

maxclique(N, Cliques) :-
  findall(X, node(X), Nodes), % all the nodes
  findall(Y, allSubsets(Nodes, Y), AllCandidates), % # find all perms of the nodes
  allCliques(AllCandidates, AllCliques), % # find all cliques
  allMaximalCliques(AllCliques, AllCliques, AllMaximalCliques), % all cliques without subset cliques
  ofLengthN(AllMaximalCliques, N, Cliques). % find all lists of length N
